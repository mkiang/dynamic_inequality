library(tidyverse)

create_new_age <- function(df) {
    df %>%
        mutate(
            age = case_when(
                age_grp == "< 1 year" ~      0,
                age_grp == "1-4 years" ~     0,
                age_grp == "5-9 years" ~     5,
                age_grp == "10-14 years" ~  10,
                age_grp == "15-19 years" ~  15,
                age_grp == "20-24 years" ~  20,
                age_grp == "25-29 years" ~  25,
                age_grp == "30-34 years" ~  30,
                age_grp == "35-39 years" ~  35,
                age_grp == "40-44 years" ~  40,
                age_grp == "45-49 years" ~  45,
                age_grp == "50-54 years" ~  50,
                age_grp == "55-59 years" ~  55,
                age_grp == "60-64 years " ~ 60,
                age_grp == "65-69 years" ~  65,
                age_grp == "70-74 years" ~  70,
                age_grp == "75-79 years" ~  75,
                age_grp == "80-84 years" ~  80,
                age_grp == "85-89 years" ~  85,
                age_grp == "90-94 years" ~  85,
                age_grp == "95-99 years" ~  85,
                age_grp == "100+ years" ~   85,
                age_grp == "All ages" ~    999,
                age_grp == "Not Stated" ~ NA_real_
            )
        )
}

import_jp <- function(f_path, ctypes = NULL) {
    readr::read_delim(f_path, delim = ";", col_types = ctypes) %>%
        janitor::clean_names(.)
}

categorize_race <- function(df) {
    df %>%
        mutate(
            race_cat = factor(
                race_eth,
                levels = c(
                    "American Indian or Alaska Native",
                    "Asian",
                    "Black or African American",
                    "hispanic",
                    "More than one race",
                    "Native Hawaiian or Other Pacific Islander",
                    "White"
                ),
                labels = c(
                    "Non-Hispanic American Indian or Alaska Native",
                    "Non-Hispanic Asian",
                    "Non-Hispanic Black",
                    "Hispanic",
                    "More than one race",
                    "Non-Hispanic Native Hawaiian or Other Pacific Islander",
                    "Non-Hispanic White"
                ),
                ordered = TRUE
            ),
            race_cat_rev = factor(
                race_eth,
                levels = rev(
                    c(
                        "American Indian or Alaska Native",
                        "Asian",
                        "Black or African American",
                        "hispanic",
                        "More than one race",
                        "Native Hawaiian or Other Pacific Islander",
                        "White"
                    )
                ),
                labels = rev(
                    c(
                        "Non-Hispanic American Indian or Alaska Native",
                        "Non-Hispanic Asian",
                        "Non-Hispanic Black",
                        "Hispanic",
                        "More than one race",
                        "Non-Hispanic Native Hawaiian or Other Pacific Islander",
                        "Non-Hispanic White"
                    )
                ),
                ordered = TRUE
            )
        ) %>% 
        mutate(
            race_short_cat = factor(
                race_eth,
                levels = c(
                    "American Indian or Alaska Native",
                    "Asian",
                    "Black or African American",
                    "hispanic",
                    "More than one race",
                    "Native Hawaiian or Other Pacific Islander",
                    "White"
                ),
                labels = c(
                    "AIAN",
                    "Asian",
                    "Black",
                    "Hispanic",
                    "Multiracial",
                    "NHOPI",
                    "White"
                ),
                ordered = TRUE
            ),
            race_short_cat_rev = factor(
                race_eth,
                levels = rev(
                    c(
                        "American Indian or Alaska Native",
                        "Asian",
                        "Black or African American",
                        "hispanic",
                        "More than one race",
                        "Native Hawaiian or Other Pacific Islander",
                        "White"
                    )
                ),
                labels = rev(
                    c(
                        "AIAN",
                        "Asian",
                        "Black",
                        "Hispanic",
                        "Multiracial",
                        "NHOPI",
                        "White"
                    )
                ),
                ordered = TRUE
            )
        )
}

make_race_eth <- function(df) {
    df %>% 
        mutate(race_eth = case_when(
              race == "aian" ~ "American Indian or Alaska Native",
              race == "asian" ~ "Asian",
              race == "black" ~ "Black or African American",
              race == "hisp" ~ "hispanic",
              race == "multirace" ~ "More than one race",
              race == "nhopi" ~ "Native Hawaiian or Other Pacific Islander",
              race == "white" ~ "White",
              TRUE ~ NA_character_
        ))
}

categorize_age_groups <- function(df) {
    df %>% 
        mutate(
            age_cat = factor(
                age_grp,
                levels = c("under65", "65andup", "all_ages"),
                labels = c("Under 65", "65 and over", "All ages"),
                ordered = TRUE
            ),
            age_cat_rev = factor(
                age_grp,
                levels = rev(c("under65", "65andup", "all_ages")),
                labels = rev(c("Under 65", "65 and over", "All ages")),
                ordered = TRUE
            )
        )
}
