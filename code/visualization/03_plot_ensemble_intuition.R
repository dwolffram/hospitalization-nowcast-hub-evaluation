source("code/src/data_utils.R")
source("code/src/plot_functions.R")

as_of1 <- "2022-04-24"
as_of2 <- "2022-08-08"

df <- load_data(
  add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE,
  fix_data = TRUE, add_truth = FALSE, exclude_missing = FALSE, eval_date = "2022-08-08"
)

df <- df %>%
  filter(model != "MedianEnsemble")

df <- df %>%
  mutate(model = fct_relevel(model, c(
    "Epiforecasts", "ILM", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble"
  )))


df_nowcasts <- df %>%
  filter(
    type == "quantile",
    forecast_date == "2022-04-24"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")

df_truth1 <- load_truth(as_of = as_of1) %>%
  rename(truth1 = truth) %>%
  filter(location == "DE")

df_truth2 <- load_truth(as_of = as_of2) %>%
  rename(truth2 = truth) %>%
  filter(location == "DE")


df_all <- df_nowcasts %>%
  right_join(df_truth1, by = c("target_end_date" = "date", "location", "age_group")) %>%
  right_join(df_truth2, by = c("target_end_date" = "date", "location", "age_group"))

alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("gray", "black"), c(as_of1, as_of2))


df1 <- df_all %>%
  filter(
    target_end_date >= as.Date("2022-03-01"),
    target_end_date <= as.Date("2022-05-15"),
    !model %in% c("MeanEnsemble", "MedianEnsemble", NA),
    age_group == "00+"
  ) %>%
  mutate(model_type = "Individual models")

df2 <- df_all %>%
  filter(
    target_end_date >= as.Date("2022-03-01"),
    target_end_date <= as.Date("2022-05-15"),
    model %in% c("MeanEnsemble"),
    age_group == "00+"
  ) %>%
  mutate(model_type = "Ensemble model")

df3 <- bind_rows(df1, df2)
df3$model_type <- factor(df3$model_type, levels = c("Individual models", "Ensemble model"))


df4 <- df_all %>%
  filter(
    target_end_date >= as.Date("2022-03-01"),
    target_end_date <= as.Date("2022-05-15"),
    age_group == "00+"
  )


ggplot(df3) +
  facet_wrap("model_type") +
  geom_vline(xintercept = as.Date(as_of1), linetype = "dashed") +
  geom_line(data = df4, aes(x = target_end_date, y = truth1, color = as_of1)) +
  geom_line(data = df4, aes(x = target_end_date, y = truth2, color = as_of2)) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%", fill = model), ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%", fill = model), ) +
  scale_alpha_manual(
    name = "Prediction interval", values = alphas,
    guide = guide_legend(order = 3, title.position = "top", title.hjust = 0)
  ) +
  scale_color_manual(
    name = "Truth", values = line_colors, labels = c("At time of nowcast", "Final"),
    guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
  ) +
  scale_fill_manual(breaks = c(
    "Epiforecasts", "ILM", "KIT",
    "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble"
  ), values = MODEL_COLORS) +
  labs(x = NULL, y = "7-day hospitalization incidence") +
  guides(fill = guide_legend(order = 2, title = "Model", ncol = 1)) +
  theme_bw() +
  theme(
    legend.position = "right", legend.box.just = "left",
    legend.direction = "vertical", legend.box = "vertical",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "lines"),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 9),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
    legend.margin = margin(0, 0, 0, 5),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent")
  )


ggsave("figures/Fig4.pdf", width = 190.5, height = 55, unit = "mm", device = "pdf")
