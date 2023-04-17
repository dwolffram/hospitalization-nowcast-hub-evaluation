source("utils.R")

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08")

df <- df %>%
  rowwise() %>%
  mutate(score = score(value, truth, type, quantile))

df <- df %>%
  select(-c(pathogen, value, truth))

write_csv(df, paste0("data/scores.csv.gz"))
