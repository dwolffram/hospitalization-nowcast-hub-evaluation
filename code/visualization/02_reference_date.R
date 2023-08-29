library(tidyverse)

dates <- factor(c(paste0("t-", 10:1), "t", paste0("t+", 1:6)), 
                levels = c(paste0("t-", 10:1), "t", paste0("t+", 1:6)))

individuals <-  rep(c("A", "B", "C", "D", "E", "F", "G", "H"), each = 3)

type <- rep(c("positive_test", "hospitalization", "report"), length((unique(individuals))))

date <- c("t-5", "t-3", "t-2",
          "t-4", "t-1", "t-1",
          "t-2", "t-2", "t-2",
          "t-3", "t+2", "t+4", 
          "t-6", "t-1", "t+2",
          "t-8", "t-3", "t-2",
          "t-9", "t-7", "t-5",
          "t+1", "t+3", "t+5")

df <- data.frame(individuals, date, type) %>% 
  mutate(highlight = date %in% c("t-6", "t-5", "t-4", "t-3", "t-2", "t-1", "t"))

highlight_cols <- setNames(c("white", "yellow"), c(FALSE, TRUE))
sizes <- setNames(c(2, 1.5, 1), c("positive_test", "hospitalization", "report"))
shapes <- setNames(c(16, 17, 15), c("positive_test", "hospitalization", "report"))
colors <- setNames(c("springgreen4", "firebrick2", "navyblue"), c("positive_test", "hospitalization", "report"))


ggplot() +
  geom_rect(data = data.frame(x = 0, y = 0), xmin = "t-6", xmax = "t", ymin = 5.5, ymax = Inf,
            alpha = 0.4, aes(fill = "outside")) +
  geom_rect(data = data.frame(x = 0, y = 0), xmin = "t-6", xmax = "t", ymin = 0.5, ymax = 5.5,
           alpha = 0.4, aes(fill = "7d_inc")) +
  geom_vline(xintercept = c("t-6", "t"), linetype = "dashed", size = 0.25) +
  geom_line(data = df, aes(x = date, y = individuals, group = individuals),
            size = 0.3) + 
  geom_point(data = df, aes(x = date, y = individuals, shape = type, size = type, color = type)) +
  scale_x_discrete(limits = dates) +
  scale_fill_manual(values = c("7d_inc" = "palegreen", "outside" = "gray"),
                    labels = c("Hospitalization included in\n7-day hosp. inc. of date t", "Not included"),
                    name = NULL,
                    guide = guide_legend(order = 2)) +
  scale_size_manual(values = sizes, guide = "none") +
  scale_color_manual(values = colors,
                     labels = c("Registration of positive test\n(reference date)", "Hospital admission", "Report of hospitalization"),
                     guide = guide_legend(order = 1)) +
  scale_shape_manual(values = shapes, 
                     labels = c("Registration of positive test\n(reference date)", "Hospital admission", "Report of hospitalization"),
                     guide = guide_legend(order = 1)) +
  labs(x = "Date", y = "Individual", shape = NULL, color = NULL) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.65, "lines"),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.major.y = element_line(size = 0.25),
    panel.grid.minor = element_line(size = 0.1),
    plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
    legend.margin = margin(0, 0, 0, 5),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent")
  )

ggsave("figures/Fig2.pdf", width = 190.5, height = 45, unit = "mm", device = "pdf")
