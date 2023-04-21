source("utils.R")
source("plot_functions.R")

SHORT_HORIZONS <- TRUE

### LOAD ALL FILES

df <- data.frame()
for (d in as.list(seq(as.Date("2022-05-10"), as.Date("2022-12-31"), by = 1))) {
  print(d)
  filename <- paste0(
    "data/scores_by_date/scores_", d,
    ifelse(SHORT_HORIZONS, "_7d", ""), ".csv.gz"
  )

  if (file.exists(filename)) {
    df_temp <- read_csv(filename, show_col_types = FALSE)
    df_temp$eval_date <- d
    df <- bind_rows(df, df_temp)
  }
}


### PLOT SCORES BY EVALUATION DATE

p1 <- plot_scores_by_eval_date(df, "national")
p2 <- plot_scores_by_eval_date(df, "states")
p3 <- plot_scores_by_eval_date(df, "age")


### COMPARISON OF DATA VERSIONS FROM 2022-08-08 AND 2022-12-31

if (!SHORT_HORIZONS) {
  df1 <- load_truth(as_of = "2022-08-08") %>%
    mutate(as_of = "2022-08-08")

  df2 <- load_truth(as_of = "2022-12-31") %>%
    mutate(as_of = "2022-12-31")

  df3 <- bind_rows(df1, df2)

  df3 <- filter_data(df3, level = "national") %>%
    filter(
      date <= "2022-04-29",
      date >= "2021-11-22"
    )

  p4 <- ggplot(df3, aes(x = date, y = truth, color = as_of)) +
    geom_line() +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
    scale_color_manual(values = c("black", "darkorange2")) +
    theme_bw() +
    labs(
      x = "Reporting date",
      y = "7-day hospitalization incidence",
      color = "Data version"
    )
}

### ASSEMBLE PLOTS

t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
  legend.title = element_text(size = 6),
  legend.text = element_text(size = 5),
  legend.key.size = unit(0.4, "lines"),
  axis.title = element_text(size = 6),
  axis.text = element_text(size = 5),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1)
))


if (!SHORT_HORIZONS) {
  # Across all horizons
  ((p1 + theme(legend.position = "none") + p2 + p3 + theme(legend.position = "none") + p4) + plot_layout(ncol = 2) & t) + plot_annotation(theme = theme(plot.margin = margin()))

  ggsave("figures/scores_by_eval_date.pdf", width = 164, height = 100, unit = "mm", device = "pdf")
} else {
  # Short horizons: 0-7 days
  (p1 + theme(legend.position = "none") + p2 + theme(legend.position = "none") + p3 & theme(aspect.ratio = 1) & t &
    scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b")) + plot_annotation(theme = theme(plot.margin = margin()))

  ggsave("figures/scores_by_eval_date_7d.pdf", width = 164, height = 50, unit = "mm", device = "pdf")
}
