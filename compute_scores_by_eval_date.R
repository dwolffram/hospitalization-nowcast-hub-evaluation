source("utils.R")

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


df <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = FALSE, exclude_missing = TRUE) %>% 
  filter(type == "quantile")


# 2022-05-10 until 2022-12-31 (steps of 10 days)
for (d in c(as.list(seq(as.Date("2022-05-10"), as.Date("2022-12-31"), by=10)), "2022-12-31")) {
  print(d)
  tryCatch(compute_scores_by_eval_date(df, d, short_horizons = FALSE), 
           error = function(e) {print(paste0("Error for date: ", d, "."))})
}
