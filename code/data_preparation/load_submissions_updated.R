source("utils.R")

START_DATE <- "2021-11-22"
END_DATE <- "2022-04-29"

PATH_UPDATED <- "../hospitalization-nowcast-hub/data-processed_updated_models/"
files <- list.files(PATH_UPDATED, pattern = "*.csv", recursive = TRUE)

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

df_files <- df_files %>%
  filter(str_detect(path, "raw", negate = TRUE))

df_files <- df_files %>%
  filter(model != "ILM-prop")

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(df_files), style = 3)
for (i in 1:nrow(df_files)) {
  row <- df_files[i, ]
  df_temp <- read_csv(paste0(PATH_UPDATED, row$path),
    show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df <- bind_rows(df, df_temp)

  setTxtProgressBar(pb, i)
}


df <- df %>%
  mutate(
    model = ifelse(str_detect(model, "Ensemble"), str_sub(model, 12, -3), str_sub(model, 1, 3)),
    retrospective = FALSE # need column 'retrospective' for fix_RKI()
  ) %>% 
  fix_RKI() %>%
  select(-retrospective)

# Exclude missing targets
files <- c(
  "Epiforecasts-missing.csv", "MeanEnsemble-missing.csv", "MedianEnsemble-missing.csv",
  "SZ-missing.csv"
)

df_missing <- data.frame()
for (f in files) {
  df_temp <- read_csv(paste0("data/submission_check/fixed/", f),
    show_col_types = FALSE, progress = FALSE
  )
  df_missing <- bind_rows(df_missing, df_temp)
}

df <- anti_join(df, df_missing)

write_csv(df, "data/submissions_updated.csv.gz")
