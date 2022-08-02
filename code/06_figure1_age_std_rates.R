## 06_figure1_age_std_rates.R ----
##
## Create Figure 1 — monthly age standardized mortality rates by race/ethnicity
## and age group.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Data ----
age_std <-
    readRDS(here::here("data", "age_standardized_death_rates.RDS")) %>%
    categorize_age_groups() %>%
    categorize_race()

## Age-standardized mortality rates plot ----
p1 <- ggplot2::ggplot(age_std,
                      ggplot2::aes(x = date,
                                   color = age_cat,
                                   group = age_cat)) +
    ggplot2::geom_rect(
        data = age_std %>% dplyr::group_by(race_med_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2020-03-15")),
            xmax = c(as.Date("2020-05-15")),
            ymin = 5,
            ymax = 829
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_rect(
        data = age_std %>% dplyr::group_by(race_med_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2020-10-15")),
            xmax = c(as.Date("2021-01-15")),
            ymin = 5,
            ymax = 829
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_rect(
        data = age_std %>% dplyr::group_by(race_med_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2021-07-15")),
            xmax = c(as.Date("2021-10-15")),
            ymin = 5,
            ymax = 829
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_rect(
        data = age_std %>% dplyr::group_by(race_med_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2021-12-01")),
            xmax = c(as.Date("2022-02-01")),
            ymin = 5,
            ymax = 829
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_vline(
        xintercept = as.Date("2020-03-01"),
        linetype = "dashed",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(
        ggplot2::aes(
            y = all_cause_rate,
            ymax = all_cause_rate + 1.96 * sqrt(all_cause_var),
            ymin = all_cause_rate - 1.96 * sqrt(all_cause_var)
        ),
        fatten = 1.25,
        alpha = .5
    ) +
    ggplot2::facet_wrap(~ race_med_cat,
                        # labeller = label_wrap_gen(width = 20, multi_line = TRUE),
                        ncol = 4) +
    ggplot2::scale_color_brewer("Age Groups", palette = "Set1") +
    ggplot2::scale_x_date(NULL,
                          breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                          labels = paste0("'", substr(2018:2022, 3, 4))) +
    ggplot2::scale_y_continuous("Monthly all-cause\nmortality rate (per 100,000)") +
    mk_nytimes(legend.position = c(.95, .05),
               legend.justification = c(1, 0)) +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "grey20"))

## Make a log version ----
p2 <- p1 +
    ggplot2::scale_y_continuous(
        "(log) Monthly all-cause\nmortality rate (per 100,000)",
        trans = "log1p",
        breaks = c(0, 10, 20, 50, 100, 200, 400, 800),
        limits = c(5, 830),
        expand = c(0, 0)
    )
ggplot2::ggsave(
    here::here("plots", "fig1_std_rates_log.pdf"),
    p2,
    width = 12, 
    height = 4.5, 
    units = "cm", 
    scale = 2.1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig1_std_rates_log.jpg"),
    p2,
    width = 12, 
    height = 4.5, 
    units = "cm", 
    scale = 2.1,
    dpi = 300
)

## Save ----
ggplot2::ggsave(
    here::here("plots", "figS1x_std_rates.pdf"),
    p1,
    width = 12, 
    height = 4.5, 
    units = "cm", 
    scale = 2.1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS1x_std_rates.jpg"),
    p1,
    width = 12, 
    height = 4.5, 
    units = "cm", 
    scale = 2.1,
    dpi = 300
)

## Save actual numbers ----
readr::write_csv(
    age_std %>%
        dplyr::select(age_cat, race_cat, date, all_cause_rate, all_cause_var),
    here::here("output", "fig1_data_std_rates.csv")
)
