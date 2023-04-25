source("code/src/data_utils.R")
library(patchwork)

# Compute frozen truth
#
# for(d in c(0, 2, 7, 14, 28, 35, 42, 49, 70, 81)){
#   print(d)
#   t <- load_frozen_truth(d, "2021-11-01")
#   write_csv(t, paste0("data/truth_frozen/truth_frozen_", d, "d.csv"))
# }

df <- data.frame()
for (delay in c(0, 2, 7, 14, 28, 35, 42, 49, 70, 81)) {
  t <- read_csv(paste0("data/truth_frozen/truth_frozen_", delay, "d.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(d = delay)
  df <- bind_rows(df, t)
}

df <- df %>%
  mutate(
    d = as_factor(d),
    location = str_sub(location, -2, -1)
  ) %>%
  rename(value = frozen_value) %>%
  filter(date >= "2021-11-22",
         date <= "2022-04-29")

df$d <- factor(df$d, levels = sort(unique(df$d)), labels = c(0, 2, 7, 14, 28, 35, 42, 49, 70, "70+"))

df_national <- df %>%
  filter(
    location == "DE",
    age_group == "00+"
  )

df_states <- df %>%
  filter(
    age_group == "00+",
    location != "DE"
  )

df_age <- df %>%
  filter(
    location == "DE",
    age_group != "00+"
  )



### PERCENTAGE (OVER TIME)
# National

df_fraction_national <- df_national %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(location, date) %>%
  mutate(value = value / max(value)) 

df_fraction_national <- df_fraction_national %>% 
  group_by(date) %>% 
  mutate(value = value - lag(value, default = 0, order_by = d))

df_fraction_national$d <- factor(df_fraction_national$d, 
                                 levels = rev(sort(unique(df_fraction_national$d))), 
                                 labels = rev(c(0, 2, 7, 14, 28, 49, 70, "70+")))

p1 <- ggplot(df_fraction_national, aes(x = date, y = value, fill = d, color = d)) +
  facet_wrap("location", scales = "fixed") +
  geom_area(position = "stack", alpha = 1, size = 0.3) +
  # scale_fill_viridis_d(direction = 1) +
  # scale_color_viridis_d(direction = 1, guide = "none") +
  scale_color_brewer(palette = "RdYlGn", guide = "none") +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw() +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  ) +
  scale_x_date(date_breaks = "1 months", minor_breaks = "1 month", date_labels = "%b", 
               expand = c(0.05, 0))


### PERCENTAGE (BY STRATUM)
# States

df_fraction <- df_states %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(location, d) %>%
  summarize(value = sum(value)) %>%
  group_by(location) %>%
  mutate(value = value / max(value))

df_fraction <- df_fraction %>%
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>%
  arrange(d)

p2 <- ggplot(df_fraction, aes(x = fct_reorder(location, value, .fun = min, .desc = TRUE), y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity", alpha = 1) +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  )


# Age

df_fraction <- df_age %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>%
  group_by(age_group, d) %>%
  summarize(value = sum(value)) %>%
  group_by(age_group) %>%
  mutate(value = value / max(value))

df_fraction <- df_fraction %>%
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>%
  arrange(d)

p4 <- ggplot(df_fraction, aes(x = age_group, y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity", alpha = 1) +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  )

# ggsave("figures/fraction_age.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


### Weekday
library(lubridate)

df_fraction <- df_national %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  mutate(weekday = wday(date, week_start = 1, label = TRUE)) %>% 
  group_by(weekday, d) %>%
  summarize(value = sum(value)) %>%
  group_by(weekday) %>%
  mutate(value = value / max(value))

df_fraction <- df_fraction %>%
  mutate(d = fct_reorder(d, value, .desc = TRUE)) %>%
  arrange(d)

p3 <- ggplot(df_fraction, aes(x = weekday, y = value, fill = d)) +
  geom_bar(stat = "identity", position = "identity") +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.05)
  ) +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  )


p1 + facet_grid( ~ "National level") + theme(legend.position = "none", 
                                             axis.text.y = element_text(size = 6)) +
  p2 + facet_grid( ~ "States") + ylab(NULL) + theme(legend.position = "none", 
                                                    axis.text.y = element_blank()) +
  p4 + facet_grid( ~ "Age groups") + ylab(NULL) + theme(legend.position = "none", 
                                                        axis.text.y = element_blank()) +
  p3 + facet_grid( ~ "Weekdays") + ylab(NULL) + theme(legend.position = "right", 
                                                      axis.text.y = element_blank()) +
  plot_layout(widths = c(1.8, 1.8, 1.25, 1)) &
  theme(legend.title = element_text(size = 6), 
        legend.text  = element_text(size = 6),
        legend.key.size = unit(0.4, "lines"),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 4),
        strip.text = element_text(size = 8),
        axis.ticks = element_line(colour = "black", size = 0.25),
        panel.grid.major = element_line(size = 0.15),
        panel.grid.minor = element_line(size = 0.1),
        plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"), 
        legend.margin = margin(0, 0, 0, 5),
        legend.box.spacing = unit(0, "pt"),
        legend.background = element_rect(fill='transparent'))

ggsave("figures/fractions.pdf", width = 164, height = 45, unit = "mm", device = "pdf")


### Over time by state

df_fraction_states <- df_states %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(location, date) %>%
  mutate(value = value / max(value))

ggplot(df_fraction_states, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("location", scales = "fixed") +
  geom_area(position = "identity") +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw() +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  ) +
  scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b") +
  theme(legend.title = element_text(size = 7), 
        legend.text  = element_text(size = 7),
        legend.key.size = unit(0.4, "lines"),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        axis.ticks = element_line(colour = "black", size = 0.25),
        panel.grid.major = element_line(size = 0.15),
        panel.grid.minor = element_line(size = 0.1),
        plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"), 
        legend.margin = margin(0, 0, 0, 5),
        legend.box.spacing = unit(0, "pt"),
        legend.background = element_rect(fill='transparent'))


ggsave("figures/fraction_time_states.pdf", width = 164, height = 150, unit = "mm", device = "pdf")



### Over time by age group

df_fraction_age <- df_age %>%
  subset(d %in% c(0, 2, 7, 14, 28, 49, 70, "70+")) %>% 
  group_by(age_group, date) %>%
  mutate(value = value / max(value))

ggplot(df_fraction_age, aes(x = date, y = value, fill = fct_reorder(d, value, .desc = TRUE)), group = d) +
  facet_wrap("age_group", scales = "fixed") +
  geom_area(position = "identity") +
  # scale_fill_viridis_d(direction = 1) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_bw() +
  labs(
    x = NULL,
    y = "Fraction of final report",
    fill = "Days after\ninitial report"
  ) +
  scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b") +
  theme(legend.title = element_text(size = 7), 
        legend.text  = element_text(size = 7),
        legend.key.size = unit(0.4, "lines"),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        axis.ticks = element_line(colour = "black", size = 0.25),
        panel.grid.major = element_line(size = 0.15),
        panel.grid.minor = element_line(size = 0.1),
        plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"), 
        legend.margin = margin(0, 0, 0, 5),
        legend.box.spacing = unit(0, "pt"),
        legend.background = element_rect(fill='transparent'))

ggsave("figures/fraction_time_age.pdf", width = 164, height = 75, unit = "mm", device = "pdf")

