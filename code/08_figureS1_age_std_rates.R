library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

age_std <- readRDS(here("data", "age_standardized_death_rates.RDS")) %>% 
    categorize_age_groups() %>% 
    categorize_race()

p1 <- ggplot(age_std, 
       aes(x = date, 
           color = race_cat, 
           group = race_cat)) +
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
    facet_grid(age_cat ~ race_cat, 
               scales = "free", 
               labeller = label_wrap_gen(width = 20, multi_line = TRUE)) + 
    scale_color_brewer("Race/Ethnicity", palette = "Dark2") + 
    scale_x_date(NULL, 
                 breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                 labels = paste0("'", substr(2018:2022, 3, 4))) + 
    scale_y_continuous("Monthly all-cause mortality rate (per 100,000)") + 
    mk_nytimes(legend.position = "none") + 
    theme(panel.border = element_rect(color = "grey20"))

p1a <- ggplot(age_std, 
             aes(x = date, 
                 color = race_cat, 
                 group = race_cat)) +
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
    facet_grid(age_cat ~ race_cat, 
               # scales = "free", 
               labeller = label_wrap_gen(width = 20, multi_line = TRUE)) + 
    scale_color_brewer("Race/Ethnicity", palette = "Dark2") + 
    scale_x_date(NULL, 
                 breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                 labels = paste0("'", substr(2018:2022, 3, 4))) + 
    scale_y_continuous("Monthly all-cause mortality rate (per 100,000)") + 
    mk_nytimes(legend.position = "none") + 
    theme(panel.border = element_rect(color = "grey20"))

ggsave(
    here("plots", "figS1_std_rates.pdf"),
    p1,
    width = 12,
    height = 5.5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS1_std_rates.jpg"),
    p1,
    width = 12,
    height = 5.5,
    scale = 1,
    dpi = 300
)
write_csv(
    age_std %>% 
        select(age_cat, race_cat, date, all_cause_rate, all_cause_var),
    here("output", "figS1_data_std_rates.csv")
)

ggsave(
    here("plots", "figS1_std_rates_fixed.pdf"),
    p1a,
    width = 12,
    height = 5.5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS1_std_rates_fixed.jpg"),
    p1a,
    width = 12,
    height = 5.5,
    scale = 1,
    dpi = 300
)
