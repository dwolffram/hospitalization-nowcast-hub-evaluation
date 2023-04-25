source("code/src/data_utils.R")
source("code/src/plot_functions.R")

### LOAD ORIGINAL NOWCASTS TOGETHER WITH UPDATED NOWCASTS

df <- load_data(
  add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE,
  fix_data = TRUE, add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08"
)

df <- df %>% filter(model %in% UPDATED_MODELS)

df_updated <- read_csv("data/submissions_updated.csv.gz", show_col_types = FALSE) %>%
  filter(!model %in% c("MeanEnsemble", "MedianEnsemble")) %>%
  mutate(model = paste(model, "(updated)"))

df_truth <- load_truth(as_of = "2022-08-08")

df_updated <- df_updated %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- bind_rows(df, df_updated)


### PLOT WIS DECOMPOSITION AND COVERAGE

p1 <- plot_wis("national", updated_models = TRUE) + theme(legend.position = "none") + labs(title = "National level")
p2 <- plot_wis("states", updated_models = TRUE) + theme(legend.position = "none") + labs(title = "States")
p3 <- plot_wis("age", updated_models = TRUE) + theme(legend.position = "right", legend.justification = "left") + labs(title = "Age groups")

p4 <- plot_coverage(df, "national", updated_models = TRUE) + theme(legend.position = "none")
p5 <- plot_coverage(df, "states", updated_models = TRUE) + theme(legend.position = "none")
p6 <- plot_coverage(df, "age", updated_models = TRUE) + theme(legend.position = "right", legend.justification = "left")

t <- list(theme(
  plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
  legend.title = element_text(size = 6),
  legend.text = element_text(size = 5),
  legend.key.size = unit(0.4, "lines"),
  axis.title = element_text(size = 7),
  axis.text = element_text(size = 6),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(0, 5, 10, 2), "pt"),
  legend.margin = margin(0, 0, 0, 4),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill = "transparent")
))

((p1 + p2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p3 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) /
  (p4 + p5 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p6 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) & t) + plot_annotation(theme = theme(plot.margin = margin()))

ggsave("figures/10_scores_updated.pdf", width = 164, height = 90, unit = "mm", device = "pdf")
