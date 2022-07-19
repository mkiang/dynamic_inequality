## 01_process_raw_death_data.R ----
## 
## Read in the CDC WONDER death files and clean them up. 

## Imports ----
library(tidyverse)
library(here)
library(janitor)
source(here::here("code", "utils.R"))

## Read in raw data ----
## Note that NCHS has changed how they handle provisional data. Specifically,
## they state:
##      Privacy policy: As of March 23, 2022, all statistics representing
##      zero through nine deaths are suppressed, in the provisional
##      mortality online database for years 2018 and later.
##
## (See: https://wonder.cdc.gov/wonder/help/mcd-provisional.html for more.)
##
## This is a departure from the standard NCHS data use agreement rules which
## specify that 0-9 deaths are *not* suppressed as long as the data are 
## national. CDC WONDER subnational data are always suppressed in cases where
## there are fewer than 10 deaths.
##
## To get the most accurate count possible, we're going to read in 2018
## through 2021 using the Feb 2022 data pull, and then append more current for
## the remaining period.
death_df <- dplyr::bind_rows(
    readr::read_delim(
        here::here(
            "data_raw",
            "age_year_month_hispanic_2018_2022_feb2022download.txt"
        )
    ) %>%
        janitor::clean_names() %>%
        dplyr::filter(is.na(notes),
                      five_year_age_groups != "Not Stated") %>%
        dplyr::transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = "hispanic",
            age_grp = five_year_age_groups,
            n_deaths = deaths
        ),
    readr::read_delim(
        here::here(
            "data_raw",
            "age_year_month_nonhisp_singlerace_2018_2022_feb2022download.txt"
        )
    ) %>%
        janitor::clean_names() %>%
        dplyr::filter(is.na(notes),
                      five_year_age_groups != "Not Stated") %>%
        dplyr::transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = single_race_6,
            age_grp = five_year_age_groups,
            n_deaths = deaths
        )
)  %>%
    dplyr::filter(year < 2022)

## Now read in a more recent data pull (as necessary)
new_death_df <- dplyr::bind_rows(
    readr::read_delim(
        here::here(
            "data_raw",
            "age_year_month_hispanic_2018_2022_jul2022download.txt"
        )
    ) %>%
        janitor::clean_names() %>%
        dplyr::filter(is.na(notes),
                      five_year_age_groups != "Not Stated") %>%
        dplyr::transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = "hispanic",
            age_grp = five_year_age_groups,
            n_deaths = as.numeric(deaths)
        ) %>%
        dplyr::filter(year >= 2022) %>%
        dplyr::filter(month <= 2),
    readr::read_delim(
        here::here(
            "data_raw",
            "age_year_month_nonhisp_singlerace_2018_2022_jul2022download.txt"
        )
    ) %>%
        janitor::clean_names() %>%
        dplyr::filter(is.na(notes),
                      five_year_age_groups != "Not Stated") %>%
        dplyr::transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = single_race_6,
            age_grp = five_year_age_groups,
            n_deaths = as.numeric(deaths)
        ) %>%
        dplyr::filter(year >= 2022) %>%
        dplyr::filter(month <= 2)
)
death_df <- dplyr::bind_rows(death_df, new_death_df)

## Add tuple info in case we expand later ----
## At some point, we may want to do this by educational attainment, geography,
## and sex, so just add those tuples now to make the rest of the code future-
## proof. 
death_df <- death_df %>%
    dplyr::mutate(
        geography = "all",
        sex = "all",
        educ = "all",
        .before = 1
    ) %>%
    dplyr::arrange(geography,
                   sex,
                   educ,
                   race_eth,
                   year,
                   month)

## Save raw data ----
saveRDS(death_df,
        here::here("data", "raw_deaths_2018_2022.RDS"),
        compress = "xz")
readr::write_csv(death_df,
                 here::here("data", "raw_deaths_2018_2022.csv"))

## Collapse into age groups that match standard pops ----
collapsed_death <- death_df %>%
    create_new_age() %>%
    dplyr::group_by(geography, sex, educ, year, month, race_eth, age) %>%
    dplyr::summarize(n_deaths = sum(n_deaths, na.rm = TRUE)) %>%
    dplyr::ungroup()

## Save collapsed data ----
saveRDS(collapsed_death,
        here::here("data", "deaths_2018_2022.RDS"),
        compress = "xz")
readr::write_csv(collapsed_death,
                 here::here("data", "deaths_2018_2022.csv"))
