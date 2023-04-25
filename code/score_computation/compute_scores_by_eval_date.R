source("code/src/data_utils.R")
source("code/src/scoring_functions.R")

df <- load_data(add_baseline = FALSE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                add_truth = FALSE, exclude_missing = TRUE) %>% 
  filter(type == "quantile")


# 2022-05-10 until 2022-12-31 (steps of 10 days)
for (d in c(as.list(seq(as.Date("2022-05-10"), as.Date("2022-12-31"), by=10)), "2022-12-31")) {
  print(d)
  tryCatch(compute_scores_by_eval_date(df, d, short_horizons = FALSE), 
           error = function(e) {print(paste0("Error for date: ", d, "."))})
}
