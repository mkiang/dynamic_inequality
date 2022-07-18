## Imports ----
library(tidyverse)
library(here)
library(janitor)
source(here("code", "utils.R"))

## Read in raw yearly data which has populations ----
pops_df <- bind_rows(
    read_delim(here(
        "data_raw", "age_year_hispanic_2018_2022_feb2022download.txt"
    )) %>%
        janitor::clean_names() %>%
        filter(!is.na(year)) %>%
        transmute(
            year = as.numeric(year_code),
            race_eth = "hispanic",
            age_grp = ifelse(
                is.na(five_year_age_groups),
                "All ages",
                five_year_age_groups), 
            pop = as.numeric(population)
        ),
    read_delim(
        here("data_raw",
             "age_year_nonhisp_singlerace_2018_2022_feb2022download.txt")
    ) %>%
        janitor::clean_names() %>%
        filter(!is.na(year)) %>% 
        transmute(
            year = as.numeric(year_code),
            race_eth = single_race_6,
            age_grp = ifelse(
                is.na(five_year_age_groups),
                "All ages",
                five_year_age_groups), 
            pop = as.numeric(population)
        )
)

### Add tuple columns in case we expand analysis later ----
pops_df <- pops_df %>% 
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
        year) %>% 
    filter(age_grp != "Not Stated")

### Save raw data ----
saveRDS(pops_df,
        here("data", "raw_pops_2018_2022.RDS"),
        compress = "xz")
write_csv(pops_df,
          here("data", "raw_pops_2018_2022.csv"))

### Fix 2022 ----
### 2022 here is just a duplicate of 2021 but depending on when you pull the
### data, sometimes it is missing population estimates. We'll just manually
### duplicate it here. 
pops_df <- bind_rows(pops_df %>%
                         filter(year < 2022),
                     pops_df %>%
                         filter(year == 2021) %>%
                         mutate(year = 2022))

## Collapse population estimates ----
collapsed_pop <- pops_df %>% 
    create_new_age() %>% 
    group_by(geography, sex, educ, year, race_eth, age) %>% 
    summarize(pop = sum(pop, na.rm = TRUE)) %>% 
    ungroup()

### Fill in the 85+ by assuming everybody who is not <85 is at least 85 ----
collapsed_pop <- collapsed_pop %>% 
    left_join(
        collapsed_pop %>% 
            filter(age < 85) %>% 
            group_by(geography, sex, educ, year, race_eth) %>% 
            summarize(under85pop = sum(pop))
    ) %>% 
    left_join(
        collapsed_pop %>% 
            filter(age == 999) %>% 
            group_by(geography, sex, educ, year, race_eth) %>% 
            summarize(totalpop = sum(pop))
    ) %>% 
    mutate(pop = ifelse(age == 85, totalpop - under85pop, pop)) %>% 
    select(-under85pop,
           -totalpop) %>% 
    filter(age != 999)

### Save collapsed data ----
saveRDS(collapsed_pop,
        here("data", "pops_2018_2022.RDS"),
        compress = "xz")
write_csv(collapsed_pop,
          here("data", "pops_2018_2022.csv"))

## Linearly extrapolate and interpolate population data ----
## Population estimates are midyear, so we need to extrapolate first six
## months of 2018 year and all of 2021 is just a repeat of 2020 data so 
## we will extrapolate all of 2021 and the first two months of 2022. 

### Get 2017 data by extrapolate backwards from 2019 and 2018 ----
pop_2017 <- collapsed_pop %>% 
    filter(year <= 2019) %>% 
    group_by(geography, sex, educ, race_eth, age) %>% 
    arrange(geography, sex, educ, race_eth, age, year) %>% 
    mutate(growth = pop / lead(pop)) %>% 
    ungroup() %>% 
    filter(!is.na(growth)) %>% 
    mutate(pop = round(growth * pop),
           year = 2017) %>% 
    select(-growth)

### Get 2021 and 2022 data by extrapolating forwards from 2019 and 2020 ----
pop_2021 <- collapsed_pop %>% 
    filter(year %in% 2019:2020) %>% 
    group_by(geography, sex, educ, race_eth, age) %>% 
    arrange(geography, sex, educ, race_eth, age, year) %>% 
    mutate(growth = pop / lag(pop)) %>% 
    ungroup() %>% 
    filter(!is.na(growth)) %>% 
    mutate(pop = round(growth * pop),
           year = 2021) %>% 
    select(-growth)

pop_2022 <- collapsed_pop %>% 
    filter(year %in% 2020:2021) %>% 
    group_by(geography, sex, educ, race_eth, age) %>% 
    arrange(geography, sex, educ, race_eth, age, year) %>% 
    mutate(growth = pop / lag(pop)) %>% 
    ungroup() %>% 
    filter(!is.na(growth)) %>% 
    mutate(pop = round(growth * growth * pop),
           year = 2022) %>% 
    select(-growth)

extrap_pop <- collapsed_pop %>% 
    filter(year != 2021) %>% 
    bind_rows(pop_2017, pop_2021, pop_2022) %>% 
    arrange(geography, sex, educ, race_eth, year, age) %>% 
    distinct()

### Then we interpolate between all those years ----
grouping_vars <- extrap_pop %>% 
    select(geography, sex, educ, race_eth, age) %>% 
    distinct()

pops_interpolated <- map_dfr(.x =  1:NROW(grouping_vars),
        .f = ~ {
            i <<- .x
            geo_x <- grouping_vars$geography[.x]
            sex_x <- grouping_vars$sex[.x]
            edu_x <- grouping_vars$educ[.x]
            rac_x <- grouping_vars$race_eth[.x]
            age_x <- grouping_vars$age[.x]
            
            holder <- NULL
            for (y in 2017:2021) {
                pop_i <- extrap_pop %>%
                    filter(
                        geography == geo_x,
                        sex == sex_x,
                        educ == edu_x,
                        race_eth == rac_x,
                        age == age_x,
                        year == y
                    ) %>%
                    pull(pop)
                
                pop_j <- extrap_pop %>%
                    filter(
                        geography == geo_x,
                        sex == sex_x,
                        educ == edu_x,
                        race_eth == rac_x,
                        age == age_x,
                        year == y + 1
                    ) %>%
                    pull(pop)
                
                holder <- bind_rows(
                    holder,
                    tibble(
                        geography = geo_x,
                        sex = sex_x,
                        educ = edu_x,
                        race_eth = rac_x,
                        age = age_x,
                        year = c(rep(y, 7), rep(y + 1, 5)),
                        month = c(6:12, 1:5),
                        pop = round(seq(pop_i, pop_j, length.out = 13)[-13])
                    )
                )
            }
            holder
        }) 

pops_interpolated <- pops_interpolated %>% 
    mutate(date = lubridate::ymd(sprintf("%s-%s-01", year, month)),
           .before = pop) %>% 
    filter(date >= as.Date("2018-01-01"),
           date <= as.Date("2022-02-01"))

### Save interpolated population estimates ----
saveRDS(pops_interpolated,
        here("data", "expanded_pops_2018_2022.RDS"),
        compress = "xz")
write_csv(pops_interpolated,
          here("data", "expanded_pops_2018_2022.csv"))
