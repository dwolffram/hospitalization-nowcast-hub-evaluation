source("utils.R")

FIX <- TRUE

# Load all submissions
df <- load_data(add_baseline = TRUE, shorten_names = TRUE,
               fix_data = FIX, exclude_missing = FALSE, add_truth = FALSE)

# All targets that should be present
template <- read_csv("data/submissions_KIT-frozen_baseline.csv.gz", show_col_types = FALSE) %>% 
  select(- c(value, model, pathogen))

# Models that cover all locations and age groups
models_all <- c("Epiforecasts", "KIT-frozen_baseline", "KIT", 
                "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "SU", "SZ")

for (m in models_all) {
  df_temp <- df %>% 
    filter(model == m)
  
  df_temp <- template %>% 
    anti_join(df_temp, by = c("location", "age_group", "forecast_date", "target_end_date", 
                              "target", "type", "quantile"))
  
  print(paste(m, ": ", nrow(df_temp), " rows missing."))
  
  if(nrow(df_temp) != 0) {
    write_csv(df_temp, paste0("data/submission_check/", ifelse(FIX,"fixed/", ""), m, "-missing.csv"))
  }
}


# Does not cover states
m <- "ILM"

df_temp <- df %>% 
  filter(model == m)

df_temp <- template %>% 
  filter(location == "DE") %>% 
  anti_join(df_temp, by = c("location", "age_group", "forecast_date", "target_end_date", 
                            "target", "type", "quantile"))

print(paste(m, ": ", nrow(df_temp), " rows missing."))
if(nrow(df_temp) != 0) {
  write_csv(df_temp, paste0("data/submission_check/", ifelse(FIX,"fixed/", ""), m, "-missing.csv"))
}


# Does not cover age groups
m <- "RKI"

df_temp <- df %>% 
  filter(model == m)

df_temp <- template %>% 
  filter(age_group == "00+") %>% 
  anti_join(df_temp, by = c("location", "age_group", "forecast_date", "target_end_date", 
                            "target", "type", "quantile"))

print(paste(m, ": ", nrow(df_temp), " rows missing."))
if(nrow(df_temp) != 0) {
  write_csv(df_temp, paste0("data/submission_check/", ifelse(FIX,"fixed/", ""), m, "-missing.csv"))
}
