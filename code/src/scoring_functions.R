# Quantile score
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
}


# Compute squared error, absolute error or quantile score based on "type"
score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    return((prediction - observation)^2)
  } else if (type == "median") {
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    return(qs(prediction, observation, quantile))
  }
}


# Compute scores for each row in a dataframe
compute_scores <- function(df) {
  df <- df %>%
    rowwise() %>%
    mutate(score = score(value, truth, type, quantile),
           score = round(score, digits = 5)) %>% 
    select(-c(pathogen, value, truth))
}


# Compute WIS decomposition
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


# Compute scores based on a chosen evaluation date, i.e., a specific truth data version
compute_scores_by_eval_date <- function(df, eval_date = "2022-08-08", short_horizons = FALSE){
  df_truth <- load_truth(as_of = eval_date)
  
  df <- df %>%
    left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
  
  if (short_horizons){
    df <- df %>% filter(target %in% paste(0:7 * -1, "day ahead inc hosp"))
  }
  
  df <- df %>%
    rowwise() %>%
    mutate(score = score(value, truth, type, quantile))
  
  df <- df %>% 
    mutate(level = ifelse(location == "DE", "national", "states"),
           level = ifelse(age_group != "00+", "age", level))
  
  df <- df %>% 
    group_by(level, model) %>% 
    summarize(score = mean(score))
  
  write_csv(df, paste0("data/scores_by_date/scores_", eval_date, 
                       ifelse(short_horizons, "_7d", ""), ".csv.gz"))
  return(df)
}