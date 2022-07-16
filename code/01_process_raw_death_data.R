## Imports ----
library(tidyverse)
library(here)
library(janitor)
source(here("code", "utils.R"))

## Read in raw data ----
## We're going to read in 2018 through 2021 using the Feb 2022 data pull,
## and then append 2022 to current using whatever new files there are. 
death_df <- bind_rows(
    read_delim(here(
        "data_raw", "age_year_month_hispanic_2018_2022_feb2022download.txt"
    )) %>%
        janitor::clean_names() %>%
        filter(is.na(notes),
               five_year_age_groups != "Not Stated") %>%
        transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = "hispanic",
            age_grp = five_year_age_groups,
            n_deaths = deaths
        ),
    read_delim(
        here(
            "data_raw",
            "age_year_month_nonhisp_singlerace_2018_2022_feb2022download.txt"
        )
    ) %>%
        janitor::clean_names() %>%
        filter(is.na(notes),
               five_year_age_groups != "Not Stated") %>%
        transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = single_race_6,
            age_grp = five_year_age_groups,
            n_deaths = deaths
        )
)  %>% 
    filter(year < 2022)

## Now read in a more recent data pull (as necessary)
new_death_df <- bind_rows(
    read_delim(here(
        "data_raw", "age_year_month_hispanic_2018_2022_may2022download.txt"
    )) %>%
        janitor::clean_names() %>%
        filter(is.na(notes),
               five_year_age_groups != "Not Stated") %>%
        transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = "hispanic",
            age_grp = five_year_age_groups,
            n_deaths = as.numeric(deaths)
        ) %>% 
        filter(year >= 2022) %>% 
        filter(month <= 2),
    read_delim(
        here(
            "data_raw",
            "age_year_month_nonhisp_singlerace_2018_2022_may2022download.txt"
        )
    ) %>%
        janitor::clean_names() %>%
        filter(is.na(notes),
               five_year_age_groups != "Not Stated") %>%
        transmute(
            year = as.numeric(year_code),
            month = as.numeric(substr(month_code, 6, 7)),
            race_eth = single_race_6,
            age_grp = five_year_age_groups,
            n_deaths = as.numeric(deaths)
        ) %>% 
        filter(year >= 2022) %>% 
        filter(month <= 2)
) 

death_df <- bind_rows(death_df, new_death_df)

## Add tuple info in case we expand later ----
## Also remove 2022 since we only have part of January
death_df <- death_df %>% 
    mutate(
        geography = "all",
        sex = "all",
        educ = "all",
        .before = 1
    ) %>% 
    arrange(
        geography,
        sex,
        educ,
        race_eth,
        year,
        month)

## Save raw data ----
saveRDS(death_df,
        here("data", "raw_deaths_2018_2022.RDS"),
        compress = "xz")
write_csv(death_df,
          here("data", "raw_deaths_2018_2022.csv"))

## Collapse into age groups that match standard pops ----
collapsed_death <- death_df %>% 
    create_new_age() %>% 
    group_by(geography, sex, educ, year, month, race_eth, age) %>% 
    summarize(n_deaths = sum(n_deaths, na.rm = TRUE)) %>% 
    ungroup()

## Save collapsed data ----
saveRDS(collapsed_death,
        here("data", "deaths_2018_2022.RDS"),
        compress = "xz")
write_csv(collapsed_death,
          here("data", "deaths_2018_2022.csv"))
