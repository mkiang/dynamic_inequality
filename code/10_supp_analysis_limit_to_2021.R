## Imports ----
library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

age_std <- read_csv(here("joinpoint",
                         "supplemental_analysis",
                         "age_standardized_death_rates.csv")) %>%
    left_join(
        import_jp(here(
            "joinpoint",
            "supplemental_analysis",
            "age_standardized_death_rates.data.txt"
        )) %>%
            select(
                age_grp, 
                race_eth,
                month_from_start,
                modeled_rate = model,
                modeled_se = standard_error
            )
    ) %>%
    left_join(
        import_jp(here(
            "joinpoint",
            "supplemental_analysis",
            "age_standardized_death_rates.ampc.txt"
        )) %>%
            select(
                age_grp, 
                race_eth,
                ampc,
                ampc_lower = ampc_c_i_low,
                ampc_upper = ampc_c_i_high,
                ampc_pval = p_value
            )
    ) %>%
    left_join(
        import_jp(here(
            "joinpoint",
            "supplemental_analysis",
            "age_standardized_death_rates.mpc.txt"
        )) %>%
            transmute(
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
    arrange(geography, sex, educ, age_cat, race_cat, date) %>% 
    zoo::na.locf(na.rm = FALSE) %>%
    left_join(
        import_jp(
            here("joinpoint",
                 "supplemental_analysis",
                 "age_standardized_death_rates.modelestimates.txt")
        ) %>%
            transmute(
                age_grp,
                race_eth,
                month_from_start = joinpoint, 
                jp_model = model,
                slope_change = slope_chg_estimate,
                slope_change_se = slope_chg_std_error,
                slope_change_pval = slope_chg_p_value
            )
    )

rr_df <- read_csv(here("joinpoint",
                       "supplemental_analysis",
                       "rate_ratios.csv")) %>%
    rename(rr_var = var) %>% 
    left_join(
        import_jp(here(
            "joinpoint",
            "supplemental_analysis",
            "race_only_rate_ratios.data.txt"
        )) %>%
            select(
                age_grp, 
                race,
                month_from_start,
                modeled_rr = model,
                modeled_se = standard_error
            )
    ) %>%
    left_join(
        import_jp(here(
            "joinpoint",
            "supplemental_analysis",
            "race_only_rate_ratios.ampc.txt"
        )) %>%
            select(
                age_grp, 
                race,
                ampc,
                ampc_lower = ampc_c_i_low,
                ampc_upper = ampc_c_i_high,
                ampc_pval = p_value
            )
    ) %>%
    left_join(
        import_jp(here(
            "joinpoint",
            "supplemental_analysis",
            "race_only_rate_ratios.mpc.txt"
        )) %>%
            transmute(
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
    arrange(geography, sex, educ, age_cat, race_cat, date) %>% 
    zoo::na.locf(na.rm = FALSE) %>%
    left_join(
        import_jp(
            here("joinpoint",
                 "supplemental_analysis",
                 "race_only_rate_ratios.modelestimates.txt")
        ) %>%
            transmute(
                age_grp,
                race,
                month_from_start = joinpoint, 
                jp_model = model,
                slope_change = slope_chg_estimate,
                slope_change_se = slope_chg_std_error,
                slope_change_pval = slope_chg_p_value
            )
    )

saveRDS(age_std, here("data", "supp_joinpoint_std_rate_results.RDS"))
saveRDS(rr_df, here("data", "supp_joinpoint_rate_ratio_results.RDS"))

## Data ----
rr_df <- rr_df %>% 
    categorize_age_groups() %>% 
    categorize_race()

## Plot ----
p1 <- ggplot(rr_df, 
             aes(x = date, 
                 color = race_cat, 
                 group = race_cat)) +
    geom_hline(yintercept = 1, 
               linetype = "solid",
               color = "black",
               alpha = .3) + 
    geom_vline(xintercept = as.Date("2020-03-01"),
               linetype = "dashed",
               alpha = .5) + 
    geom_pointrange(
        aes(
            y = rr,
            ymax = rr_upper,
            ymin = rr_lower
        ),
        fatten = 1.25,
        alpha = .2
    ) +
    geom_line(aes(y = modeled_rr),
              size = 1,
              alpha = .8) + 
    facet_wrap(~ age_cat, 
               ncol = 3) + 
    scale_color_brewer("Race/ethnicity", palette = "Dark2") + 
    scale_x_date(NULL, 
                 breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                 labels = paste0("'", substr(2018:2022, 3, 4))) + 
    scale_y_continuous("Rate Ratio\n(relative to non-Hispanic white)",
                       trans = "log",
                       breaks = c(.5, 1, 2)) + 
    mk_nytimes(
        legend.position = "right", 
        legend.justification = c(1, 0)
    ) + 
    theme(panel.border = element_rect(color = "grey20"))

## Save ---- 
ggsave(
    here("plots", "figS3_supp_rate_ratios.pdf"),
    p1,
    width = 7,
    height = 2.2,
    scale = 1.5,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS3_supp_rate_ratios.jpg"),
    p1,
    width = 7,
    height = 2.2,
    scale = 1.5,
    dpi = 200
)
