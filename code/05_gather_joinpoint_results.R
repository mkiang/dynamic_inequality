## 05_gather_joinpoint_results.R ----
##
## After running the joinpoint regression program, producing results, and
## exporting the results, this file will gather up all results and prepare
## them for plotting.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## Read in joinpoint export files ----
age_std <-
    readr::read_csv(
        here::here("joinpoint", "age_standardized_death_rates.csv")
        ) %>%
    dplyr::left_join(
        import_jp(
            here::here("joinpoint", "age_standardized_death_rates.data.txt")
        ) %>%
            dplyr::select(
                age_grp,
                race_eth,
                month_from_start,
                modeled_rate = model,
                modeled_se = standard_error
            )
    ) %>%
    dplyr::left_join(
        import_jp(
            here::here("joinpoint", "age_standardized_death_rates.ampc.txt")
        ) %>%
            dplyr::select(
                age_grp,
                race_eth,
                ampc,
                ampc_lower = ampc_c_i_low,
                ampc_upper = ampc_c_i_high,
                ampc_pval = p_value
            )
    ) %>%
    dplyr::left_join(
        import_jp(
            here::here("joinpoint", "age_standardized_death_rates.mpc.txt")
        ) %>%
            dplyr::transmute(
                age_grp,
                race_eth,
                jp_model = model,
                segment = segment + 1,
                month_from_start = segment_start,
                mpc,
                mpc_lower = mpc_95_percent_lcl,
                mpc_upper = mpc_95_percent_ucl,
                mpc_pval = p_value
            )
    ) %>%
    categorize_race() %>%
    categorize_age_groups() %>%
    dplyr::arrange(geography, sex, educ, age_cat, race_cat, date) %>%
    zoo::na.locf(na.rm = FALSE) %>%
    dplyr::left_join(
        import_jp(
            here::here(
                "joinpoint",
                "age_standardized_death_rates.modelestimates.txt"
            )
        ) %>%
            dplyr::transmute(
                age_grp,
                race_eth,
                month_from_start = joinpoint,
                jp_model = model,
                slope_change = slope_chg_estimate,
                slope_change_se = slope_chg_std_error,
                slope_change_pval = slope_chg_p_value
            )
    )

rr_df <-
    readr::read_csv(here::here("joinpoint", "rate_ratios.csv")) %>%
    dplyr::rename(rr_var = var) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "race_only_rate_ratios.data.txt"
        )) %>%
            dplyr::select(
                age_grp,
                race,
                month_from_start,
                modeled_rr = model,
                modeled_se = standard_error
            )
    ) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "race_only_rate_ratios.ampc.txt"
        )) %>%
            dplyr::select(
                age_grp,
                race,
                ampc,
                ampc_lower = ampc_c_i_low,
                ampc_upper = ampc_c_i_high,
                ampc_pval = p_value
            )
    ) %>%
    dplyr::left_join(
        import_jp(here::here(
            "joinpoint", "race_only_rate_ratios.mpc.txt"
        )) %>%
            dplyr::transmute(
                age_grp,
                race,
                jp_model = model,
                segment = segment + 1,
                month_from_start = segment_start,
                mpc,
                mpc_lower = mpc_95_percent_lcl,
                mpc_upper = mpc_95_percent_ucl,
                mpc_pval = p_value
            )
    ) %>%
    make_race_eth() %>%
    categorize_race() %>%
    categorize_age_groups() %>%
    dplyr::arrange(geography, sex, educ, age_cat, race_cat, date) %>%
    zoo::na.locf(na.rm = FALSE) %>%
    dplyr::left_join(
        import_jp(
            here::here("joinpoint", "race_only_rate_ratios.modelestimates.txt")
        ) %>%
            dplyr::transmute(
                age_grp,
                race,
                month_from_start = joinpoint,
                jp_model = model,
                slope_change = slope_chg_estimate,
                slope_change_se = slope_chg_std_error,
                slope_change_pval = slope_chg_p_value
            )
    )

## Save ----
saveRDS(age_std,
        here::here("data", "joinpoint_std_rate_results.RDS"))
saveRDS(rr_df,
        here::here("data", "joinpoint_rate_ratio_results.RDS"))
