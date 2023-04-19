source("utils.R")
source("plot_functions.R")

### OVERALL WIS AND WIS BY HORIZON

p1 <- plot_wis("national")
p2 <- plot_scores("quantile", "national", by_horizon = TRUE)
p2b <- plot_scores("quantile", "national", by_horizon = TRUE, relative = TRUE)

p3 <- plot_wis("states")
p4 <- plot_scores("quantile", "states", by_horizon = TRUE)
p4b <- plot_scores("quantile", "states", by_horizon = TRUE, relative = TRUE)

p5 <- plot_wis("age")
p6 <- plot_scores("quantile", "age", by_horizon = TRUE)
p6b <- plot_scores("quantile", "age", by_horizon = TRUE, relative = TRUE)

t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, margin = margin(10, 0, -3, 0), face = "bold"),
  legend.title = element_text(size = 6),
  legend.text = element_text(size = 5),
  legend.key.size = unit(0.4, "lines"),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 2, 2, 2), "pt"),
  legend.margin = margin(4, 0, 0, 0),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill = "transparent")
))

(p1 + p2 + labs(title = "National level") + p2b) /
  (p3 + p4 + labs(title = "States") + p4b) /
  (p5 + p6 + labs(title = "Age groups") + p6b) + plot_annotation(theme = theme(plot.margin = margin())) & t

ggsave("figures/scores_wis.pdf", width = 164, height = 200, unit = "mm", device = "pdf")
