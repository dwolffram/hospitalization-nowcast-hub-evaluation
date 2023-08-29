source("code/src/data_utils.R")
library(patchwork)
Sys.setlocale("LC_ALL", "C")


df_40d <- read_csv("data/truth_40d.csv.gz") %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  mutate(type = "40d")

df_final <- load_truth(as_of = "2022-08-08") %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  mutate(type = "final")

df1 <- load_truth(as_of = "2021-12-01") %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  mutate(type = "t")

df2 <- load_truth(as_of = "2022-01-10") %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  mutate(type = "t + 40")

df3 <- load_truth(as_of = "2022-03-01") %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  mutate(type = "t")

df4 <- load_truth(as_of = "2022-04-10") %>%
  filter(
    location == "DE",
    age_group == "00+"
  ) %>%
  mutate(type = "t + 40")

df_a <- bind_rows(
  df_final,
  df1,
  df2
) %>%
  filter(
    date >= "2021-10-25",
    date <= "2022-04-29"
  )

df_b <- bind_rows(
  df_final,
  df3,
  df4
) %>%
  filter(
    date >= "2021-10-25",
    date <= "2022-04-29"
  )

# Load frozen truth
df_frozen <- read_csv("data/truth_frozen/truth_frozen_0d_long.csv", show_col_types = FALSE) %>%
  filter(
    location == "DE",
    age_group == "00+",
    date >= "2021-10-25",
    date <= "2022-04-29"
  ) %>%
  mutate(type = "frozen") %>%
  rename(truth = frozen_value)

p1 <- ggplot(df_a, aes(x = date, y = truth, color = type)) +
  geom_vline(xintercept = as.Date("2021-12-01"), linetype = "dashed") +
  geom_line() +
  geom_point(data = subset(df_a, date == "2021-12-01" & type == "t + 40"), show.legend = FALSE) +
  geom_point(data = subset(df_a, date == "2021-12-01" & type == "t"), show.legend = FALSE) +
  scale_color_manual(
    values = c("#000000", "#E69F00", "#009E73"),
    labels = c(
      "Original target",
      "Data reported on day t",
      "Data reported on day t + 40 days"
    ),
    guide = guide_legend(order = 1)
  ) +
  theme_bw() +
  labs(x = NULL, y = "7-day hospitalization incidence", color = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.65, "lines"),
    axis.title.y = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    legend.background = element_rect(fill = "transparent")
  ) +
  expand_limits(y = c(2000, 13000))


p2 <- ggplot(df_b, aes(x = date, y = truth, color = type)) +
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "dashed") +
  geom_line() +
  geom_point(data = subset(df_b, date == "2022-03-01" & type == "t + 40"), show.legend = FALSE) +
  geom_point(data = subset(df_b, date == "2022-03-01" & type == "t"), show.legend = FALSE) +
  scale_color_manual(
    values = c("#000000", "#E69F00", "#009E73"),
    labels = c(
      "Original target",
      "Data reported on day t",
      "Data reported on day t + 40 days"
    ),
    guide = guide_legend(order = 1)
  ) +
  theme_bw() +
  labs(x = NULL, y = "7-day hospitalization incidence", color = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.65, "lines"),
    axis.title.y = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    legend.background = element_rect(fill = "transparent")
  ) +
  expand_limits(y = c(2000, 13000))


df_c <- bind_rows(
  df_final,
  df_40d,
  df_frozen
) %>%
  filter(
    date >= "2021-10-25",
    date <= "2022-04-29"
  )

p3 <- ggplot(df_c, aes(x = date, y = truth, color = type)) +
  geom_vline(xintercept = as.Date(c("2021-12-01", "2022-03-01")), linetype = "dashed") +
  geom_line() +
  geom_point(data = subset(df_c, date == "2021-12-01" & type == "40d"), show.legend = FALSE) +
  geom_point(data = subset(df_c, date == "2022-03-01" & type == "40d"), show.legend = FALSE) +
  geom_point(data = subset(df_c, date == "2021-12-01" & type == "frozen"), show.legend = FALSE) +
  geom_point(data = subset(df_c, date == "2022-03-01" & type == "frozen"), show.legend = FALSE) +
  scale_color_manual(
    values = c("#009E73", "#000000", "#E69F00"),
    labels = c(
      "Alternative target with a\nmaximum delay of 40 days",
      "Original target: data\nreported on 2022-08-08",
      "unrevised, initial reports\n(\"frozen values\")"
    ),
    guide = guide_legend(order = 1)
  ) +
  theme_bw() +
  labs(x = NULL, y = "7-day hospitalization incidence", color = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.65, "lines"),
    axis.title.y = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.ticks = element_line(colour = "black", size = 0.25),
    panel.grid.major = element_line(size = 0.15),
    panel.grid.minor = element_line(size = 0.1),
    legend.background = element_rect(fill = "transparent")
  ) +
  expand_limits(y = c(2000, 13000))

p3 / p1 / p2


ggsave("figures/Fig_A3.pdf", width = 164, height = 140, unit = "mm", device = "pdf")
