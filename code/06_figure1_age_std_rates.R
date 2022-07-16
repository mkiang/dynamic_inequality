library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

age_std <- readRDS(here("data", "age_standardized_death_rates.RDS")) %>% 
    categorize_age_groups() %>% 
    categorize_race()

p1 <- ggplot(age_std, 
       aes(x = date, 
           color = age_cat, 
           group = age_cat)) +
    geom_vline(xintercept = as.Date("2020-03-01"),
               linetype = "dashed",
               alpha = .5) + 
    geom_pointrange(
        aes(
            y = all_cause_rate,
            ymax = all_cause_rate + 1.96 * sqrt(all_cause_var),
            ymin = all_cause_rate - 1.96 * sqrt(all_cause_var)
        ), 
        fatten = 1.25, 
        alpha = .5
    ) + 
    facet_wrap(~ race_short_cat, 
               # labeller = label_wrap_gen(width = 20, multi_line = TRUE),
               ncol = 4) + 
    scale_color_brewer("Age Groups", palette = "Set1") + 
    scale_x_date(NULL, 
                 breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                 labels = paste0("'", substr(2018:2022, 3, 4))) + 
    scale_y_continuous("Monthly all-cause mortality rate (per 100,000)") + 
    mk_nytimes(legend.position = c(.95, .05),
               legend.justification = c(1, 0)) + 
    theme(panel.border = element_rect(color = "grey20"))

ggsave(
    here("plots", "fig1_std_rates.pdf"),
    p1,
    width = 9,
    height = 3.5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig1_std_rates.jpg"),
    p1,
    width = 9,
    height = 3.5,
    scale = 1,
    dpi = 300
)
write_csv(
    age_std %>% 
        select(age_cat, race_cat, date, all_cause_rate, all_cause_var),
    here("output", "fig1_data_std_rates.csv")
)

p2 <- p1 + 
    scale_y_continuous("(log) Monthly all-cause mortality rate (per 100,000)",
                        trans = "log1p",
                        breaks = c(0, 10, 25, 50, 100, 250, 500, 750)) 
ggsave(
    here("plots", "fig1_std_rates_log.pdf"),
    p2,
    width = 9,
    height = 3.5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig1_std_rates_log.jpg"),
    p2,
    width = 9,
    height = 3.5,
    scale = 1,
    dpi = 300
)
