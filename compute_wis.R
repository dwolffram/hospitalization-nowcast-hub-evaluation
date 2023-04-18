source("utils.R")

compute_wis <- function(df) {
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    rename(med = value) %>%
    select(-c(quantile, pathogen, retrospective, truth))

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
write_csv(df_national, paste0("data/wis_national.csv.gz"))

df_states <- filter_data(df, level = "states")
df_states <- compute_wis(df_states)
write_csv(df_states, paste0("data/wis_states.csv.gz"))

df_age <- filter_data(df, level = "age")
df_age <- compute_wis(df_age)
write_csv(df_age, paste0("data/wis_age.csv.gz"))


### SHORT HORIZONS: 0-7 DAYS

df_national_7d <- filter_data(df, level = "national", short_horizons = TRUE)
df_national_7d <- compute_wis(df_national_7d)
write_csv(df_national_7d, paste0("data/wis_national_7d.csv.gz"))

df_states_7d <- filter_data(df, level = "states", short_horizons = TRUE)
df_states_7d <- compute_wis(df_states_7d)
write_csv(df_states_7d, paste0("data/wis_states_7d.csv.gz"))

df_age_7d <- filter_data(df, level = "age", short_horizons = TRUE)
df_age_7d <- compute_wis(df_age_7d)
write_csv(df_age_7d, paste0("data/wis_age_7d.csv.gz"))


### PER 100K POPULATION
