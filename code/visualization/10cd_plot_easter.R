source("code/src/data_utils.R")

### EASTER: NATIONAL LEVEL

df3 <- load_truth(as_of = "2022-04-19") %>% 
  filter(location == "DE",
         age_group == "00+") %>% 
  mutate(as_of = "2022-04-19",
         version = "initial")

df_final3 <- load_truth(as_of = "2022-08-08") %>% 
  filter(location == "DE",
         age_group == "00+") %>% 
  mutate(as_of = "2022-08-08",
         version = "final")

df_frozen3 <- read_csv("data/truth_frozen/truth_frozen_0d.csv") %>% 
  filter(location == "DE",
         age_group == "00+") %>% 
  mutate(version = "frozen") %>% 
  rename(truth = frozen_value)

df_all3 <- bind_rows(df3, df_final3, df_frozen3) %>% 
  filter(date >= "2022-03-01",
         date <= "2022-06-01")

df_ensemble3 <- load_nowcast("NowcastHub-MeanEnsemble", "2022-04-19")

ALPHAS <- setNames(c(0.7, 0.4), c("50%", "95%"))

p3 <- ggplot() +
  geom_line(data = df_all3, aes(x = date, y = truth, color = version), size = 0.25) +
  geom_vline(xintercept = as.Date("2022-04-19"), color = "black", linetype = "dashed", size = 0.25) +
  geom_ribbon(data = df_ensemble3, 
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, 
                  alpha = "95%"), 
              fill = "skyblue3") +
  geom_ribbon(data = df_ensemble3, 
              aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, 
                  alpha = "50%"), 
              fill = "skyblue3") +
  geom_line(data = df_ensemble3, aes(x = target_end_date, y = quantile_0.5), 
            color = "navyblue", size = 0.25, linetype = "solid") +
  labs(title = "Germany", x = NULL, y = "7-day hospitalization incidence", color = "Data version") +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = "2 months", minor_breaks = "1 month") +
  scale_color_manual(breaks = c("initial", "final", "frozen"), values = c("firebrick3", "black", "gray")) +   
  scale_alpha_manual(name = "Ensemble nowcast with \nprediction intervals:", values = ALPHAS,
                     guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)) +
  theme(text = element_text(size = 26),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 16),
        aspect.ratio = 1) 

# ggsave("figures/09c_issues_easter.pdf", width = 300, height = 200, unit = "mm", device = "pdf")


### EASTER: LOWER SAXONY, ISSUE OF EPIFORECASTS

df4 <- load_truth(as_of = "2022-04-20") %>% 
  filter(location == "DE-NI",
         age_group == "00+") %>% 
  mutate(as_of = "2022-04-20",
         version = "initial")

df_final4 <- load_truth(as_of = "2022-08-08") %>% 
  filter(location == "DE-NI",
         age_group == "00+") %>% 
  mutate(as_of = "2022-08-08",
         version = "final")

df_frozen4 <- read_csv("data/truth_frozen/truth_frozen_0d.csv") %>% 
  filter(location == "DE-NI",
         age_group == "00+") %>% 
  mutate(version = "frozen") %>% 
  rename(truth = frozen_value)

df_all4 <- bind_rows(df4, df_final4, df_frozen4) %>% 
  filter(date >= "2022-03-15",
         date <= "2022-05-01")

df_mean <- load_nowcast("NowcastHub-MeanEnsemble", "2022-04-20", location = "DE-NI", age_group = "00+") %>% 
  mutate(model = "MeanEnsemble")
df_median <- load_nowcast("NowcastHub-MedianEnsemble", "2022-04-20", location = "DE-NI", age_group = "00+") %>% 
  mutate(model = "MedianEnsemble")
df_epi <- load_nowcast("Epiforecasts-independent", "2022-04-20", location = "DE-NI", age_group = "00+") %>% 
  mutate(model = "Epiforecasts")

df_nowcasts <- bind_rows(df_mean, df_epi)


MODEL_FILL <- setNames(c("#B30000", "#009E73", "yellow"), c("Epiforecasts", "MeanEnsemble", "MedianEnsemble"))

p4 <- ggplot() +
  geom_line(data = df_all4, aes(x = date, y = truth, color = version), size = 0.25) +
  geom_vline(xintercept = as.Date("2022-04-20"), color = "black", linetype = "dashed", size = 0.25) +
  geom_ribbon(data = df_epi,
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975,
                 fill = "Epiforecasts"), alpha = 0.4) +
  geom_ribbon(data = df_mean,
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, 
                  fill = "MeanEnsemble"), alpha = 0.6) +
  geom_ribbon(data = df_median,
              aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, 
                  fill = "MedianEnsemble"), alpha = 0.8) +
  # geom_line(data = df_epi, aes(x = target_end_date, y = quantile_0.5, group = as_of), 
  #           color = "#B30000", size = 0.5, linetype = "solid") +
  # geom_line(data = df_mean, aes(x = target_end_date, y = quantile_0.5, group = as_of), 
  #           color = "#009E73", size = 0.5, linetype = "solid") +
  # geom_line(data = df_median, aes(x = target_end_date, y = quantile_0.5, group = as_of), 
  #           color = "yellow", size = 0.5, linetype = "solid") +
  labs(title = "Lower Saxony", x = NULL, y = "7-day hospitalization incidence", color = "Data version") +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = "1 month") +
  scale_color_manual(breaks = c("initial", "final", "frozen"), values = c("darkorange4", "black", "gray")) +   
  scale_fill_manual(name = "Model", values = MODEL_FILL,
                     guide = guide_legend(order = 3, title.position = "top", title.hjust = 0)) +
  theme(text = element_text(size = 26),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 16),
        aspect.ratio = 1) +
  coord_cartesian(ylim = c(0, 2000))

# ggsave("figures/09d_issues_easter_epi.pdf", width = 300, height = 200, unit = "mm", device = "pdf")
