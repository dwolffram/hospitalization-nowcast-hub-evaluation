library(patchwork)
source("code/visualization/10a_plot_saxony.R")
source("code/visualization/10b_plot_bremen.R")
source("code/visualization/10cd_plot_easter.R")


(p1 + p2) /
  (p3 + p4) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.65, "lines"),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
    legend.margin = margin(0, 0, 0, 5),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent"),
    plot.tag = element_text(size = 10, face = "bold")
  )

ggsave("figures/Fig10.pdf", width = 190, height = 140, unit = "mm", device = "pdf")
