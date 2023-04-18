source("utils.R")

compute_wis <- function(df) {
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    rename(med = value) %>%
    select(-any_of(c("quantile", "pathogen", "retrospective", "truth")))

  df <- df %>%
    filter(type == "quantile") %>%
    left_join(df_median)

  df <- df %>%
    rowwise() %>%
    mutate(
      score = score(value, truth, type, quantile),
      spread = score(value, med, type, quantile),
      overprediction = ifelse(med > truth, score - spread, 0),
      underprediction = ifelse(med < truth, score - spread, 0)
    )

  df <- df %>%
    group_by(model) %>%
    summarize(
      spread = mean(spread),
      overprediction = mean(overprediction),
      underprediction = mean(underprediction),
      score = mean(score)
    )

  return(df)
}


df <- load_data(
  add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
  add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08"
)


### OVERALL

df_national <- filter_data(df, level = "national")
df_national <- compute_wis(df_national)
write_csv(df_national, "data/wis_national.csv")

df_states <- filter_data(df, level = "states")
df_states <- compute_wis(df_states)
write_csv(df_states, "data/wis_states.csv")

df_age <- filter_data(df, level = "age")
df_age <- compute_wis(df_age)
write_csv(df_age, "data/wis_age.csv")


### SHORT HORIZONS: 0-7 DAYS

df_national_7d <- filter_data(df, level = "national", short_horizons = TRUE)
df_national_7d <- compute_wis(df_national_7d)
write_csv(df_national_7d, "data/wis_national_7d.csv")

df_states_7d <- filter_data(df, level = "states", short_horizons = TRUE)
df_states_7d <- compute_wis(df_states_7d)
write_csv(df_states_7d, "data/wis_states_7d.csv")

df_age_7d <- filter_data(df, level = "age", short_horizons = TRUE)
df_age_7d <- compute_wis(df_age_7d)
write_csv(df_age_7d, "data/wis_age_7d.csv")


### PER 100K POPULATION

df <- load_data(
  add_baseline = TRUE, add_median = TRUE, shorten_names = TRUE, fix_data = TRUE,
  add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08", per_100k = TRUE
)

df_national_100k <- filter_data(df, level = "national")
df_national_100k <- compute_wis(df_national_100k)
write_csv(df_national_100k, "data/wis_national_100k.csv")

df_states_100k <- filter_data(df, level = "states")
df_states_100k <- compute_wis(df_states_100k)
write_csv(df_states_100k, "data/wis_states_100k.csv")

df_age_100k <- filter_data(df, level = "age")
df_age_100k <- compute_wis(df_age_100k)
write_csv(df_age_100k, "data/wis_age_100k.csv")


### UPDATED MODELS

df <- read_csv("data/submissions_updated.csv.gz") %>%
  filter(!model %in% c("MeanEnsemble", "MedianEnsemble"))

df_truth <- load_truth(as_of = "2022-08-08")

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))

df_national <- filter_data(df, level = "national")
df_national <- compute_wis(df_national)
write_csv(df_national, "data/wis_national_updated.csv")

df_states <- filter_data(df, level = "states")
df_states <- compute_wis(df_states)
write_csv(df_states, "data/wis_states_updated.csv")

df_age <- filter_data(df, level = "age")
df_age <- compute_wis(df_age)
write_csv(df_age, "data/wis_age_updated.csv")
