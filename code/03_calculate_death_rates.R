## 03_calculate_death_rates.R ----
##
## Use the death and population data to calculate age-standardized mortality
## rates by race/ethnicity and age groups.

## Imports ----
library(tidyverse)
library(here)
library(narcan)

## Data ----
pop_df <- readRDS(here::here("data", "expanded_pops_2018_2022.RDS"))
death_df <- readRDS(here::here("data", "deaths_2018_2022.RDS"))

## Analytic dataset with standard pops ----
analytic_df <- dplyr::left_join(pop_df,
                                death_df) %>%
    narcan::add_std_pop()

## Age specific death rates ----
age_spec_death <- analytic_df %>%
    narcan::calc_asrate_var(all_cause, n_deaths)

## Age standardized death rates ----
age_std_death <- dplyr::bind_rows(
    age_spec_death %>%
        dplyr::mutate(age_grp = "all_ages"),
    age_spec_death %>%
        dplyr::filter(age < 65) %>%
        dplyr::mutate(age_grp = "under65"),
    age_spec_death %>%
        dplyr::filter(age >= 65) %>%
        dplyr::mutate(age_grp = "65andup")
) %>%
    dplyr::group_by(geography,
                    sex,
                    educ,
                    age_grp, 
                    race_eth, 
                    year, 
                    month,
                    date) %>%
    dplyr::arrange(geography,
                   sex,
                   educ,
                   age_grp, 
                   race_eth, 
                   year, 
                   month,
                   date, 
                   age) %>%
    narcan::calc_stdrate_var(all_cause_rate, all_cause_var) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(month_from_start = (year - min(year)) * 12 + month)

## Save ----
saveRDS(analytic_df,
        here::here("data", "analytic_df.RDS"))
saveRDS(age_spec_death,
        here::here("data", "age_specific_death_rates.RDS"))
saveRDS(age_std_death,
        here::here("data", "age_standardized_death_rates.RDS"))
readr::write_csv(age_std_death,
                 here::here("data", "age_standardized_death_rates.csv"))

## Save results for joinpoint analyses ----
readr::write_csv(age_std_death,
                 here::here("joinpoint", "age_standardized_death_rates.csv"))
readr::write_csv(
    age_std_death %>%
        dplyr::filter(year < 2022),
    here::here(
        "joinpoint",
        "supplemental_analysis",
        "age_standardized_death_rates.csv"
    )
)
