source("code/src/data_utils.R")
source("code/src/plot_functions.R")

df <- load_data(
  add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE,
  fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08"
)

p1 <- plot_coverage(df, "national")
p2 <- plot_coverage(df, "states")
p3 <- plot_coverage(df, "age")

p4 <- plot_coverage_lines(df, "national")
p5 <- plot_coverage_lines(df, "states")
p6 <- plot_coverage_lines(df, "age")

t <- list(theme(
  plot.title = element_text(size = 10, hjust = 0, face = "bold"),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  legend.key.size = unit(0.4, "lines"),
  strip.text = element_text(size = 9),
  axis.title = element_text(size = 9),
  axis.text = element_text(size = 8),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 7, 2, 2), "pt"),
  legend.margin = margin(4, 0, 0, 0),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill = "transparent")
))

wrap_elements(p1 + p4 + labs(title = "National level") + theme(legend.position = "none") + plot_layout(widths = c(1, 2)) + plot_annotation(theme = theme(plot.margin = margin())) & theme(aspect.ratio = 1) & t) /
  wrap_elements(p2 + p5 + labs(title = "States") + theme(legend.position = "none") + plot_layout(widths = c(1, 2)) + plot_annotation(theme = theme(plot.margin = margin())) & theme(aspect.ratio = 1) & t) /
  wrap_elements(p3 + p6 + labs(title = "Age groups") + theme(legend.position = "none") + plot_layout(widths = c(1, 2)) + plot_annotation(theme = theme(plot.margin = margin())) & theme(aspect.ratio = 1) & t)

ggsave("figures/Fig8.pdf", width = 190.5, height = 220, unit = "mm", device = "pdf")
