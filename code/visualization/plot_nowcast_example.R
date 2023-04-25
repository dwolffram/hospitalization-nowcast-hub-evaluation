source("code/src/data_utils.R")

DATES <- c("2021-12-01", "2022-02-01", "2022-04-01", "2022-08-08")

# Load nowcasts
df_nowcast <- data.frame()
for (d in DATES[-4]) {
  df_temp <- load_nowcast("NowcastHub-MeanEnsemble", d, "DE", "00+")
  df_temp$as_of <- d
  df_nowcast <- bind_rows(df_nowcast, df_temp)
}

# Load different data versions
df_truth <- data.frame()
for (d in DATES) {
  df_temp <- load_truth(as_of = d) %>%
    filter(location == "DE", age_group == "00+")
  df_temp$as_of <- d
  df_truth <- bind_rows(df_truth, df_temp)
}

# Compute and save frozen truth
# truth_frozen <- load_frozen_truth(0, "2021-04-01")
# write_csv(truth_frozen, "data/truth_frozen/truth_frozen_0d_long.csv")

# Load frozen truth
df_frozen <- read_csv("data/truth_frozen/truth_frozen_0d_long.csv", show_col_types = FALSE) %>%
  filter(
    location == "DE",
    age_group == "00+",
    date <= "2022-06-01",
    date >= "2021-07-01"
  ) %>%
  mutate(as_of = "unrevised, \ninitial reports") %>%
  rename(truth = frozen_value)

df_truth <- bind_rows(df_truth, df_frozen) %>%
  filter(date >= "2021-07-01", date <= "2022-06-01")


### PLOT

ALPHAS <- setNames(c(0.7, 0.4), c("50%", "95%"))

ggplot(df_truth) +
  geom_vline(
    data = data.frame(date = as.Date(DATES[-4])),
    aes(xintercept = date, linetype = "Date of nowcast"), color = "black"
  ) +
  geom_line(aes(x = date, y = truth, color = as_of)) +
  geom_ribbon(
    data = df_nowcast,
    aes(
      x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
      group = as_of, alpha = "95%"
    ),
    fill = "skyblue3"
  ) +
  geom_ribbon(
    data = df_nowcast,
    aes(
      x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75,
      group = as_of, alpha = "50%"
    ),
    fill = "skyblue3"
  ) +
  geom_line(
    data = df_nowcast, aes(x = target_end_date, y = quantile_0.5, group = as_of),
    color = "skyblue3", linetype = "solid"
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00", "#000000", "gray"), guide = guide_legend(order = 1)) +
  scale_linetype_manual(name = NULL, values = c("Date of nowcast" = "dotted")) +
  theme_bw() +
  labs(
    x = NULL,
    y = "7-day hospitalization incidence",
    color = "Data version"
  ) +
  scale_alpha_manual(name = "Nowcasts with \nprediction intervals:", values = ALPHAS) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.65, "lines"),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
    legend.margin = margin(0, 0, 0, 5),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent")
  ) +
  expand_limits(x = as.Date("2022-06-10"), y = 12750)

ggsave("figures/nowcast_example.pdf", width = 164, height = 55, unit = "mm", device = "pdf")
