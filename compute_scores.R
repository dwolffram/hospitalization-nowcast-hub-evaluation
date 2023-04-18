source("utils.R")

compute_scores <- function(df) {
  df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile),
         score = round(score, digits = 5)) %>% 
  select(-c(pathogen, value, truth))
}


### OVERALL

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df_scores <- compute_scores(df)

write_csv(df_scores, "data/scores.csv.gz")


### PER 100K POPULATION

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08", per_100k = TRUE)

df_scores <- compute_scores(df)

write_csv(df_scores, "data/scores_100k.csv.gz")


### UPDATED MODELS

df <- read_csv("data/submissions_updated.csv.gz") %>% 
  filter(! model %in% c("MeanEnsemble", "MedianEnsemble"))

df_truth <- load_truth(as_of = "2022-08-08")

df_median <- df %>%
  filter(quantile == 0.5) %>%
  mutate(type = "median")

df <- bind_rows(df, df_median) %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- compute_scores(df)

write_csv(df, "data/scores_updated.csv.gz")


### TRUTH: 40 DAYS 

# df_truth <- load_frozen_truth(lead_time = 40, start_date = "2021-10-01") %>%
#   rename(truth = frozen_value)
# 
# write_csv(df_truth, "data/truth_40d.csv.gz")

df_truth <- read_csv("data/truth_40d.csv.gz")

df <- load_data(add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = FALSE, exclude_missing = TRUE)
df <- df %>% 
  filter(model != "ILM",
         model != "MeanEnsemble",
         model != "MedianEnsemble")

df_updated <- read_csv("data/submissions_updated.csv.gz") %>% 
  filter(model %in% c("ILM", "MeanEnsemble", "MedianEnsemble"))

df <- bind_rows(df, df_updated)

df_median <- df %>%
  filter(quantile == 0.5) %>%
  mutate(type = "median")

df <- bind_rows(df, df_median) %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df <- compute_scores(df)

write_csv(df, "data/scores_40d.csv.gz")
