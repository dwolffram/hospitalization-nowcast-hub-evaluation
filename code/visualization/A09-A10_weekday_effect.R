source("code/src/data_utils.R")
library(lubridate)

dates <- seq(
  from = as.Date("2021-11-22"),
  to = as.Date("2022-04-29"),
  by = "1 days"
)

df_truth <- load_truth(as_of = "2022-08-08") %>%
  filter(
    location == "DE",
    age_group == "00+",
    date >= "2021-10-25",
    date <= "2022-04-29"
  ) %>%
  mutate(type = "final")


line_colors <- setNames(
  c("skyblue3", "firebrick3"),
  c("Nowcasts (median)", "Truth")
)


dfs <- data.frame()
for (d in as.list(dates)) {
  df <- load_nowcast("KIT-simple_nowcast", d) %>%
    mutate(
      as_of = d,
      day_of_week = wday(d, label = TRUE, week_start = 1, abbr = FALSE)
    )
  dfs <- bind_rows(dfs, df)
}


ggplot(dfs) +
  facet_wrap("day_of_week") +
  geom_line(data = df_truth, aes(x = date, y = truth, color = "Truth")) +
  geom_line(aes(x = target_end_date, y = quantile_0.5, group = as_of, color = "Nowcasts (median)"),
    size = 0.25
  ) +
  scale_color_manual(values = line_colors) +
  theme_bw() +
  labs(
    x = NULL, y = "7-day hospitalization incidence", color = NULL,
    title = "Model: KIT"
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 8),
    strip.text = element_text(size = 8, margin = margin(b = 2, t = 2)),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    legend.position = c(0.65, 0.1),
    legend.background = element_rect(fill = "transparent"), # transparent legend bg
    # legend.box.background = element_rect(fill='transparent')
    # legend.justification = c(1, 0)
  ) +
  expand_limits(y = c(4000))

ggsave("figures/Fig_A09.pdf", width = 164, height = 70, unit = "mm", device = "pdf")



dfs <- data.frame()
for (d in as.list(dates)) {
  tryCatch(
    {
      df <- load_nowcast("ILM-prop", d) %>%
        mutate(
          as_of = d,
          day_of_week = wday(d, label = TRUE, week_start = 1, abbr = FALSE)
        )
      dfs <- bind_rows(dfs, df)
    },
    error = function(e) {
      print(paste0("Error for date: ", d, "."))
    }
  )
}

ggplot(dfs) +
  facet_wrap("day_of_week") +
  geom_line(data = df_truth, aes(x = date, y = truth, color = "Truth")) +
  geom_line(aes(x = target_end_date, y = quantile_0.5, group = as_of, color = "Nowcasts (median)"),
    size = 0.25
  ) +
  scale_color_manual(values = line_colors) +
  theme_bw() +
  labs(
    x = NULL, y = "7-day hospitalization incidence", color = NULL,
    title = "Model: ILM"
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 8),
    strip.text = element_text(size = 8, margin = margin(b = 2, t = 2)),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    legend.position = c(0.65, 0.1),
    legend.background = element_rect(fill = "transparent"), # transparent legend bg
    # legend.justification = c(1, 0)
  ) +
  expand_limits(y = c(4000))

ggsave("figures/Fig_A10.pdf", width = 164, height = 70, unit = "mm", device = "pdf")
