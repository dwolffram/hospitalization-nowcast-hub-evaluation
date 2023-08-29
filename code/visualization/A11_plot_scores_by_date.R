source("code/src/data_utils.R")
source("code/src/plot_functions.R")

df <- load_scores()

p1 <- plot_scores_by_date(df, "national")
p2 <- plot_scores_by_date(df, "states")
p3 <- plot_scores_by_date(df, "age")

t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, margin = margin(10, 0, 5, 0), face = "bold"),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 6),
  legend.key.size = unit(0.4, "lines"),
  strip.text = element_text(size = 8),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 2, 2, 20), "pt")
))

((p1 + theme(legend.position = "none")) /
  p2 /
  (p3 + theme(legend.position = "none"))) +
  plot_annotation(theme = theme(plot.margin = margin())) & t

ggsave("figures/Fig_A11.pdf", width = 164, height = 200, unit = "mm", device = "pdf")
