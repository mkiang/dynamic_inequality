## 07_figure2_ratios.R ----
##
## Plot monthly rate ratios with observed points (and their 95% CIs) and the
## joinpoint model fit.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Data ----
rr_df <-
    readRDS(here::here("data", "joinpoint_rate_ratio_results.RDS")) %>%
    categorize_age_groups() %>%
    categorize_race()

## Plot ----
p1 <- ggplot2::ggplot(rr_df,
                      ggplot2::aes(x = date,
                                   color = race_cat,
                                   group = race_cat)) +
    ggplot2::geom_rect(
        data = rr_df %>% dplyr::group_by(age_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2020-03-15")),
            xmax = c(as.Date("2020-05-15")),
            ymin = .3,
            ymax = 2.7
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_rect(
        data = rr_df %>% dplyr::group_by(age_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2020-10-15")),
            xmax = c(as.Date("2021-01-15")),
            ymin = .3,
            ymax = 2.7
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_rect(
        data = rr_df %>% dplyr::group_by(age_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2021-07-15")),
            xmax = c(as.Date("2021-10-15")),
            ymin = .3,
            ymax = 2.7
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_rect(
        data = rr_df %>% dplyr::group_by(age_cat) %>% dplyr::slice(1),
        ggplot2::aes(
            x = NULL,
            color = NULL,
            group = NULL,
            xmin = c(as.Date("2021-12-01")),
            xmax = c(as.Date("2022-02-01")),
            ymin = .3,
            ymax = 2.7
        ),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::geom_hline(
        yintercept = 1,
        linetype = "solid",
        color = "black",
        alpha = .3
    ) +
    ggplot2::geom_vline(
        xintercept = as.Date("2020-03-01"),
        linetype = "dashed",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(
        ggplot2::aes(y = rr,
                     ymax = rr_upper,
                     ymin = rr_lower),
        fatten = 1.25,
        alpha = .2
    ) +
    ggplot2::geom_line(ggplot2::aes(y = modeled_rr),
                       size = 1,
                       alpha = .8) +
    ggplot2::facet_wrap(~ age_cat,
                        ncol = 3) +
    ggplot2::scale_color_brewer("Race/ethnicity", palette = "Dark2") +
    ggplot2::scale_x_date(NULL,
                          breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                          labels = paste0("'", substr(2018:2022, 3, 4))) +
    ggplot2::scale_y_continuous(
        "Rate Ratio\n(relative to non-Hispanic white)",
        trans = "log",
        breaks = c(.5, 1, 2),
        limits = c(.3, 2.7),
        expand = c(0, 0)
    ) +
    mk_nytimes(legend.position = "right",
               legend.justification = c(1, 0)) +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "grey20"))

## Save ----
ggplot2::ggsave(
    here::here("plots", "fig2_rate_ratios.pdf"),
    p1,
    width = 12, 
    height = 4, 
    units = "cm", 
    scale = 2.1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig2_rate_ratios.jpg"),
    p1,
    width = 12, 
    height = 4, 
    units = "cm", 
    scale = 2.1,
    dpi = 300
)

## Save a CSV ----
readr::write_csv(
    rr_df %>%
        dplyr::select(age_cat, race_cat, date, rr, rr_lower, rr_upper, modeled_rr),
    here::here("output", "fig2_data_rate_ratios.csv")
)
