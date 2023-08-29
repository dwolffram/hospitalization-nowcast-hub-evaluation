source("code/src/data_utils.R")

df_ensemble2 <- load_nowcast("NowcastHub-MeanEnsemble", "2022-01-11", location = "DE-HB")

df2 <- load_truth(as_of = "2022-01-11") %>%
  filter(
    location == "DE-HB",
    age_group == "00+"
  ) %>%
  mutate(
    as_of = "2022-01-11",
    version = "2022-01-11"
  )

df_corrected2 <- load_truth(as_of = "2022-01-13") %>%
  filter(
    location == "DE-HB",
    age_group == "00+"
  ) %>%
  mutate(version = "2022-01-13")

df_final2 <- load_truth(as_of = "2022-08-08") %>%
  filter(
    location == "DE-HB",
    age_group == "00+"
  ) %>%
  mutate(
    as_of = "2022-08-08",
    version = "2022-08-08 (final)"
  )

df_all2 <- bind_rows(df2, df_final2, df_corrected2) %>%
  filter(
    date >= "2021-12-15",
    date <= "2022-01-15"
  )

ALPHAS <- setNames(
  c(0.7, 0.4, 1, 1, 1),
  c("50%", "95%", "2022-01-11", "2022-01-13", "2022-08-08 (final)")
)

p2 <- ggplot() +
  geom_vline(xintercept = as.Date("2022-01-11"), color = "black", linetype = "dashed", size = 0.25) +
  geom_ribbon(
    data = df_ensemble2,
    aes(
      x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
      alpha = "95%"
    ),
    fill = "skyblue3"
  ) +
  geom_ribbon(
    data = df_ensemble2,
    aes(
      x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
      alpha = "50%"
    ),
    fill = "skyblue3"
  ) +
  geom_line(
    data = df_ensemble2, aes(x = target_end_date, y = quantile_0.5, size = "Ensemble nowcast\nas of 2022-01-11"),
    color = "navyblue", linetype = "solid"
  ) +
  geom_line(data = df_all2, aes(x = date, y = truth, color = version, group = as_of, alpha = version), size = 0.25) +
  labs(title = "Bremen", x = NULL, y = "7-day hospitalization incidence", color = "Data version") +
  theme_bw() +
  scale_x_date(breaks = "1 month", date_labels = "%Y-%m-%d", minor_breaks = "1 day") +
  scale_color_manual(
    breaks = c("2022-01-11", "2022-01-13", "2022-08-08 (final)"),
    values = c("firebrick3", "springgreen4", "black"), guide = guide_legend(order = 1)
  ) +
  scale_size_manual(NULL, values = c(0.25), guide = guide_legend(order = 2)) +
  scale_alpha_manual(values = ALPHAS, guide = "none") +
  theme(
    text = element_text(size = 26),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 16),
    aspect.ratio = 1
  )

# ggsave("figures/09b_issues_bremen.pdf", width = 300, height = 200, unit = "mm", device = "pdf")
