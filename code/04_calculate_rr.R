## Imports ----
library(tidyverse)
library(here)

## Data ----
age_spec_death <- readRDS(here("data", "age_specific_death_rates.RDS"))

## Age-specific rates for all cause deaths ----
## From Flanders, 1984 Eq. 5, we need:
##      (w_i/t_i)^2 x_i
## For all comparison groups
##      i is age group, w is weight, t is person-years, and x is cases
## 
## Let's do it long first and call it all_cause_wtx
age_spec_death <- age_spec_death %>% 
    dplyr::mutate(
        all_cause_rate = n_deaths / pop, 
        all_cause_wtx  = (unit_w / pop)^2 * n_deaths
    )

age_spec_death <- bind_rows(
    age_spec_death %>% 
        mutate(age_grp = "all_ages"),
    age_spec_death %>% 
        filter(age < 65) %>% 
        mutate(age_grp = "under65"), 
    age_spec_death %>% 
        filter(age >= 65) %>% 
        mutate(age_grp = "65andup")
) 

## Age standardized rates for all cause deaths ----
## From Flanders, 1984 Eq. 5, we need:
##      the sum of all_cause_wtx  
##      the age standardized rates
## Additionally, we need the inverse of the standardized rate, squared:
##      (1/std_rate)^2 * \sum{(w_i/t_i)^2 x_i} for all groups
age_std_death <- age_spec_death %>% 
    dplyr::group_by(geography, sex, educ, age_grp, race_eth, year, month, date) %>% 
    dplyr::summarize(
        dras_all_cause  = weighted.mean(all_cause_rate, unit_w),
        all_cause_wtx = sum(all_cause_wtx)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(inv_rate_wtx = (1/dras_all_cause)^2 * all_cause_wtx)

## Then switch to wide format ----
age_std_wide <- age_std_death %>% 
    mutate(race_eth = 
               case_when(
                   race_eth == "American Indian or Alaska Native" ~ "aian",
                   race_eth == "Asian" ~ "asian", 
                   race_eth == "Black or African American" ~ "nhb", 
                   race_eth == "hispanic" ~ "hisp", 
                   race_eth == "More than one race" ~ "multirace", 
                   race_eth == "Native Hawaiian or Other Pacific Islander" ~ "nhopi", 
                   race_eth == "White" ~ "white"
               )) %>% 
    pivot_wider(
        id_cols = c(geography, sex, educ, age_grp, year, month, date),
        names_from = race_eth,
        values_from = c(dras_all_cause, inv_rate_wtx)
    ) 

death_rr <- age_std_wide %>% 
    transmute(
        geography,
        sex,
        educ,
        age_grp, 
        year,
        month,
        date,
        aian_rr = dras_all_cause_aian / dras_all_cause_white,
        aian_var = aian_rr^2 * (inv_rate_wtx_aian + inv_rate_wtx_white),
        aian_sd = sqrt(aian_var),
        asian_rr = dras_all_cause_asian / dras_all_cause_white,
        asian_var = asian_rr^2 * (inv_rate_wtx_asian + inv_rate_wtx_white),
        asian_sd = sqrt(asian_var),
        black_rr = dras_all_cause_nhb / dras_all_cause_white,
        black_var = black_rr^2 * (inv_rate_wtx_nhb + inv_rate_wtx_white),
        black_sd = sqrt(black_var),
        hisp_rr = dras_all_cause_hisp / dras_all_cause_white,
        hisp_var = hisp_rr^2 * (inv_rate_wtx_hisp + inv_rate_wtx_white),
        hisp_sd = sqrt(hisp_var),
        multirace_rr = dras_all_cause_multirace / dras_all_cause_white,
        multirace_var = multirace_rr^2 * (inv_rate_wtx_multirace + inv_rate_wtx_white),
        multirace_sd = sqrt(multirace_var),
        nhopi_rr = dras_all_cause_nhopi / dras_all_cause_white,
        nhopi_var = nhopi_rr^2 * (inv_rate_wtx_nhopi + inv_rate_wtx_white),
        nhopi_sd = sqrt(nhopi_var)
    )

rr_long <- death_rr %>%
    select(geography:date, ends_with("_rr")) %>%
    pivot_longer(cols = ends_with("_rr"),
                 names_to = "race",
                 values_to = "rr") %>% 
    mutate(race = gsub("_rr", "", race, fixed = TRUE)) %>% 
    left_join(
        death_rr %>%
            select(geography:date, ends_with("_var")) %>%
            pivot_longer(cols = ends_with("_var"),
                         names_to = "race",
                         values_to = "var") %>% 
            mutate(race = gsub("_var", "", race, fixed = TRUE))
    ) %>% 
    left_join(
        death_rr %>%
            select(geography:date, ends_with("_sd")) %>%
            pivot_longer(cols = ends_with("_sd"),
                         names_to = "race",
                         values_to = "sd") %>% 
            mutate(race = gsub("_sd", "", race, fixed = TRUE))
    ) %>% 
    arrange(geography, sex, educ, age_grp, race, date) %>% 
    mutate(month_from_start = (year - min(year)) * 12 + month)

## From Flanders 1984, eq 6, calculate the bounds of the RR ----
## Upper / Lower = RR * exp(+/-1.96 * sqrt(V))
## where V = variance we estimated above divided by RR^2.
rr_long <- rr_long %>% 
    mutate(rr_lower = rr * exp(-1.96 * sqrt(var/(rr^2))),
           rr_upper = rr * exp(1.96 * sqrt(var/(rr^2))))

write_csv(rr_long,
          here("data", "rate_ratios.csv"))
saveRDS(rr_long,
        here("data", "rate_ratios.RDS"))

write_csv(rr_long,
          here("joinpoint", "rate_ratios.csv"))
