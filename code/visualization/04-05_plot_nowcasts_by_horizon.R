source("code/src/data_utils.R")

Sys.setlocale("LC_ALL", "C")

df <- load_data(
  add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
  add_truth = TRUE, exclude_missing = FALSE, eval_date = "2022-08-08"
)

LEAD_TIME <- 0

truth_frozen <- load_frozen_truth(LEAD_TIME, "2021-11-01")

if (LEAD_TIME == 0) {
  t <- "0 day ahead inc hosp"
} else {
  t <- paste0("-", LEAD_TIME, " day ahead inc hosp")
}

df1 <- df %>%
  filter(
    target == t,
    type == "quantile",
    location == "DE",
    age_group == "00+"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")


df1 <- df1 %>%
  left_join(truth_frozen, by = c("target_end_date" = "date", "location", "age_group")) %>%
  drop_na(frozen_value)

df1$age_group <- factor(df1$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))


alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("firebrick3", "gray"), c("Final", "At time of nowcast"))

df1 <- df1 %>%
  mutate(model = fct_relevel(model, c(
    "Epiforecasts", "ILM", "KIT",
    "LMU",
    "RIVM", "RKI", "SU", "SZ", "KIT-frozen_baseline", "MeanEnsemble", "MedianEnsemble"
  )), model = fct_recode(model, FrozenBaseline = "KIT-frozen_baseline"))


ggplot(df1) +
  facet_wrap("model", scales = "fixed", ncol = 3) +
  geom_line(aes(x = target_end_date, y = frozen_value, color = "At time of nowcast"), linetype = "solid") +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
    fill = "skyblue3"
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
    fill = "skyblue3"
  ) +
  geom_line(aes(x = target_end_date, y = quantile_0.5),
    color = "royalblue4", size = 0.25, linetype = "solid"
  ) +
  geom_line(aes(x = target_end_date, y = truth, color = "Final")) +
  labs(x = NULL, y = "7-day hospitalization incidence", title = paste("Horizon:", LEAD_TIME, "days")) +
  scale_alpha_manual(
    name = "Nowcasts with \nprediction intervals:", values = alphas,
    guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
  ) +
  scale_color_manual(
    name = "Truth", values = line_colors,
    guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
  ) +
  scale_y_continuous(breaks = c(5000, 10000, 15000)) +
  scale_x_date(breaks = c(as.Date("2021-12-01"), as.Date("2022-02-01"), as.Date("2022-04-01")), minor_breaks = "1 month", date_labels = "%b %Y") +
  # expand_limits(y = 0) +
  expand_limits(x = c(as.Date("2021-11-22"), as.Date("2022-04-29"))) +
  theme_bw() +
  {
    if (LEAD_TIME == 0) coord_cartesian(ylim = c(NA, 17500))
  } + # only used for LEAD_TIME = 0
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.65, "lines"),
    strip.text = element_text(size = 8, margin = margin(b = 2, t = 2)),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
    legend.margin = margin(0, 0, 0, 5),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent")
  )

ggsave(paste0("figures/Fig", ifelse(LEAD_TIME == 0, "5", "6"), ".pdf"), width = 190.5, height = 110, unit = "mm", device = "pdf")
