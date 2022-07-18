## Imports ----
library(tidyverse)
library(here)
library(narcan)

## Data ----
pop_df <- readRDS(here("data", "expanded_pops_2018_2022.RDS"))
death_df <- readRDS(here("data", "deaths_2018_2022.RDS"))

## Analytic dataset with standard pops ----
analytic_df <- left_join(
    pop_df,
    death_df
) %>% 
    narcan::add_std_pop()

## Age specific death rates ----
age_spec_death <- analytic_df %>% 
    calc_asrate_var(all_cause, n_deaths)

## Age standardized death rates ----
age_std_death <- bind_rows(
    age_spec_death %>% 
        mutate(age_grp = "all_ages"),
    age_spec_death %>% 
        filter(age < 65) %>% 
        mutate(age_grp = "under65"), 
    age_spec_death %>% 
        filter(age >= 65) %>% 
        mutate(age_grp = "65andup")
) %>% 
    group_by(geography, sex, educ, age_grp, race_eth, year, month, date) %>% 
    arrange(geography, sex, educ, age_grp, race_eth, year, month, date, age) %>% 
    calc_stdrate_var(all_cause_rate, all_cause_var) %>% 
    ungroup() %>% 
    mutate(month_from_start = (year - min(year)) * 12 + month)

## Save ----
saveRDS(analytic_df,
        here("data", "analytic_df.RDS"))
saveRDS(age_spec_death,
        here("data", "age_specific_death_rates.RDS"))
saveRDS(age_std_death,
        here("data", "age_standardized_death_rates.RDS"))
write_csv(age_std_death,
        here("data", "age_standardized_death_rates.csv"))

## Save results for joinpoint analyses ----
write_csv(age_std_death,
          here("joinpoint", "age_standardized_death_rates.csv"))
write_csv(age_std_death %>% 
              filter(year < 2022),
          here("joinpoint", "supplemental_analysis", "age_standardized_death_rates.csv"))
