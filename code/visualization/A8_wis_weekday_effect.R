source("code/src/data_utils.R")
source("code/src/plot_functions.R")
library(lubridate)

df <- load_scores(short_horizons = FALSE, per_100k = FALSE, updated_models = FALSE,
                  truth_40d = FALSE, load_baseline = TRUE)

df_wday <- filter_data(df, level = "national") %>%
  filter(type == "quantile",
         model != "KIT-frozen_baseline") %>% 
  # filter(target == "0 day ahead inc hosp") %>%
  mutate(weekday = wday(forecast_date, label = TRUE, week_start = 1)) %>%
  group_by(model, weekday) %>%
  summarize(score = mean(score))


ggplot(df_wday, aes(x = weekday, y = score, fill = model)) +
  facet_wrap("model", scales = "fixed", ncol = 5) +
  geom_col(width = 0.8) +
  theme_bw() +
  scale_fill_manual(values = MODEL_COLORS, guide = "none") +
  labs(
    x = "Weekday",
    y = "Mean WIS"#,
    #title = "National level"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.65, "lines"),
    strip.text = element_text(size = 8, margin = margin(b = 2, t = 2)),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 5),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.major.y = element_line(size = 0.25),
    panel.grid.minor = element_line(size = 0.1),
    plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
    legend.margin = margin(0, 0, 0, 5),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent")
  )

ggsave("figures/A8_wis_weekday_national.pdf", width = 164, height = 60, unit = "mm", device = "pdf")
