source("code/src/data_utils.R")
source("code/src/plot_functions.R")

### ADJUSTED TRUTH: MAXIMUM DELAY OF 40 DAYS

## WIS

p1 <- plot_wis("national", truth_40d = TRUE) + theme(legend.position = "none") + labs(title = "National level")
p2 <- plot_wis("states", truth_40d = TRUE) + theme(legend.position = "none") + labs(title = "States")
p3 <- plot_wis("age", truth_40d = TRUE) + theme(legend.position = "right", legend.justification = "left") + labs(title = "Age groups")


# plot_wis("national", truth_40d = TRUE) 

# ggsave("figures/scores40d.pdf", width = 120, height = 100, unit = "mm", device = "pdf")

## COVERAGE

# Load new truth data (with a maximum delay of 40 days)
df_truth <- read_csv("data/truth_40d.csv.gz", show_col_types = FALSE)

# Load submissions and replace ILM, MeanEnsemble and MedianEnsemble with the adjusted versions
df <- load_data(
  add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
  add_truth = FALSE, exclude_missing = TRUE
)
df <- df %>%
  filter(
    model != "ILM",
    model != "MeanEnsemble",
    model != "MedianEnsemble"
  )

df_updated <- read_csv("data/submissions_updated.csv.gz", show_col_types = FALSE) %>%
  filter(model %in% c("ILM", "MeanEnsemble", "MedianEnsemble"))

df <- bind_rows(df, df_updated) %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))


p4 <- plot_coverage(df, "national") + theme(legend.position = "none")
p5 <- plot_coverage(df, "states") + theme(legend.position = "none")
p6 <- plot_coverage(df, "age") + theme(legend.position = "right", legend.justification = "left")

t <- list(theme(
  plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 8),
  legend.key.size = unit(0.4, "lines"),
  axis.title = element_text(size = 8),
  axis.text = element_text(size = 7),
  axis.ticks = element_line(colour = "black", size = 0.25),
  panel.grid.major = element_line(size = 0.15),
  panel.grid.minor = element_line(size = 0.1),
  plot.margin = unit(c(2, 2, 10, 2), "pt"),
  legend.margin = margin(0, 0, 0, 4),
  legend.box.spacing = unit(0, "pt"),
  legend.background = element_rect(fill = "transparent")
))

((p1 + p2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p3 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) /
  (p4 + p5 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + p6 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())) & t) + plot_annotation(theme = theme(plot.margin = margin()))

ggsave("figures/Fig13.pdf", width = 190.5, height = 110, unit = "mm", device = "pdf")
