library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

rr_df <- readRDS(here("data", "joinpoint_rate_ratio_results.RDS")) %>% 
    categorize_age_groups() %>% 
    categorize_race()

p1 <- ggplot(rr_df, 
             aes(x = date, 
                 color = age_cat, 
                 group = age_cat)) +
    geom_hline(yintercept = 1, 
               linetype = "solid",
               color = "black",
               alpha = .3) + 
    geom_vline(xintercept = as.Date("2020-03-01"),
               linetype = "dashed",
               alpha = .5) + 
    # geom_pointrange(
    #     aes(
    #         y = rr,
    #         ymax = rr_upper,
    #         ymin = rr_lower
    #     ), 
    #     fatten = 1.25, 
    #     alpha = .3
    # ) + 
    geom_line(aes(y = modeled_rr),
              size = 1,
              alpha = .8) + 
    facet_wrap(~ race_short_cat, 
               # labeller = label_wrap_gen(width = 20, multi_line = TRUE), 
               ncol = 4) + 
    scale_color_brewer("Age Groups", palette = "Set1") + 
    scale_x_date(NULL, 
                 breaks = as.Date(sprintf("%s-01-01", 2018:2022)),
                 labels = paste0("'", substr(2018:2022, 3, 4))) + 
    scale_y_continuous("Rate Ratio (relative to non-Hispanic white)") + 
    mk_nytimes(legend.position = c(.95, .05),
               legend.justification = c(1, 0)) + 
    theme(panel.border = element_rect(color = "grey20"))

ggsave(
    here("plots", "fig2_rate_ratios.pdf"),
    p1,
    width = 9,
    height = 3.5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig2_rate_ratios.jpg"),
    p1,
    width = 9,
    height = 3.5,
    scale = 1,
    dpi = 300
)
write_csv(
    rr_df %>% 
        select(age_cat, race_cat, date, rr, rr_lower, rr_upper, modeled_rr),
    here("output", "fig2_data_rate_ratios.csv")
)
