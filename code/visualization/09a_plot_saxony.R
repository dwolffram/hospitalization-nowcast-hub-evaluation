source("code/src/data_utils.R")

df <- load_truth(as_of = "2021-11-22") %>%
  filter(
    location == "DE-SN",
    age_group == "00+"
  ) %>%
  mutate(
    as_of = "2021-11-22",
    version = "initial"
  )

df_final <- load_truth(as_of = "2022-08-08") %>%
  filter(
    location == "DE-SN",
    age_group == "00+"
  ) %>%
  mutate(
    as_of = "2022-08-08",
    version = "final"
  )

truth_frozen <- load_frozen_truth(start_date = "2021-10-01")
df_frozen <- truth_frozen %>%
  filter(
    location == "DE-SN",
    age_group == "00+"
  ) %>%
  mutate(version = "frozen") %>%
  rename(truth = frozen_value)

df_all <- bind_rows(df, df_final, df_frozen) %>%
  filter(
    date >= "2021-10-01",
    date <= "2021-12-01"
  )

df_ensemble <- load_nowcast("NowcastHub-MeanEnsemble", "2021-11-22", location = "DE-SN")

ALPHAS <- setNames(c(0.7, 0.4), c("50%", "95%"))

p1 <- ggplot() +
  geom_line(data = df_all, aes(x = date, y = truth, color = version), size = 0.25) +
  geom_vline(xintercept = as.Date("2021-11-22"), color = "black", linetype = "dashed", size = 0.25) +
  geom_ribbon(
    data = df_ensemble,
    aes(
      x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
      alpha = "95%"
    ),
    fill = "skyblue3"
  ) +
  geom_ribbon(
    data = df_ensemble,
    aes(
      x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
      alpha = "50%"
    ),
    fill = "skyblue3"
  ) +
  geom_line(
    data = df_ensemble, aes(x = target_end_date, y = quantile_0.5),
    color = "navyblue", size = 0.25, linetype = "solid"
  ) +
  labs(title = "Saxony", x = NULL, y = "7-day hospitalization incidence", color = "Data version") +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = "1 month") +
  scale_color_manual(breaks = c("initial", "final", "frozen"), values = c("firebrick3", "black", "gray")) +
  scale_alpha_manual(
    name = "Ensemble nowcast with \nprediction intervals:", values = ALPHAS,
    guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
  ) +
  theme(
    text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    aspect.ratio = 1
  )

# ggsave("figures/09a_issues_saxony2.pdf", width = 80, height = 50, unit = "mm", device = "pdf")
