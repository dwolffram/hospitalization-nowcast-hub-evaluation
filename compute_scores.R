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

write_csv(df_scores, paste0("data/scores.csv.gz"))


### PER 100K POPULATION

df <- load_data(add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08", per_100k = TRUE)

df_scores <- compute_scores(df)

write_csv(df_scores, paste0("data/scores_100k.csv.gz"))
