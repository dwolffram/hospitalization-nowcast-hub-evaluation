source("code/src/data_utils.R")
source("code/src/plot_functions.R")

df <- load_scores(load_baseline = FALSE)

p1 <- plot_ranks(df, "national")
p2 <- plot_ranks(df, "states")
p3 <- plot_ranks(df, "age")

t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0, face = "bold"),
  legend.title = element_text(size = 6),
  legend.text = element_text(size = 5),
  legend.key.size = unit(0.8, "lines"),
  strip.text = element_text(size = 8),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 7, 2, 2), "pt"),
  legend.background = element_rect(fill = "transparent")
))

(p1 + ylab(NULL) + theme(legend.position = "None") + facet_grid(~"National level") +
  p2 + ylab(NULL) + theme(legend.position = "bottom") + facet_grid(~"States") +
  p3 + ylab(NULL) + theme(legend.position = "None") + facet_grid(~"Age groups")) & t

ggsave("figures/Fig_A07.pdf", width = 164, height = 100, unit = "mm", device = "pdf")
