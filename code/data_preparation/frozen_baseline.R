source("load_truth.R")

make_template <- function(forecast_date) {
  location1 <- c("DE")
  location2 <- c(
    "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE",
    "DE-HH", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL",
    "DE-SN", "DE-ST", "DE-TH"
  )

  age_group1 <- c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+")
  age_group2 <- c("00+")

  target <- paste(-28:0, "day ahead inc hosp")

  t <- data.frame(
    type = c("mean", rep("quantile", 7)),
    quantile = c(NA, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  )

  pathogen <- c("COVID-19")

  df1 <- crossing(location1, age_group1, forecast_date, target, t, pathogen) %>%
    rename(
      location = location1,
      age_group = age_group1
    )

  df2 <- crossing(location2, age_group2, forecast_date, target, t, pathogen) %>%
    rename(
      location = location2,
      age_group = age_group2
    )

  df <- bind_rows(df1, df2)

  df <- df %>%
    mutate(
      horizon = as.numeric(str_extract(target, "-?\\d+")),
      target_end_date = as.Date(forecast_date) + as.numeric(horizon)
    ) %>%
    select(-horizon)

  return(df)
}


create_frozen_baseline <- function(forecast_date) {
  df <- make_template(forecast_date)

  df_frozen <- load_truth(forecast_date)

  df_frozen <- df_frozen %>%
    filter(date >= as.Date(forecast_date) - 28) %>%
    rename(
      value = truth,
      target_end_date = date
    )

  df <- df %>%
    left_join(df_frozen, by = c("location", "age_group", "target_end_date"))

  df <- df %>%
    mutate(age_group = fct_relevel(age_group, c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))) %>%
    select(location, age_group, forecast_date, target_end_date, target, type, quantile, value, pathogen) %>%
    arrange(location, age_group, forecast_date, target_end_date)
  return(df)
}

# df <- create_frozen_baseline("2022-07-17")


# The following needs to be run with several start_dates
# Github doesn't allow that many request and will throw an HTTP error 403 after a while

start_date <- as.Date("2021-11-22")
end_date <- as.Date("2022-04-29")
dates <- as.character(seq(start_date, end_date, "days"))

for (date in dates) {
  print(date)
  df <- create_frozen_baseline(date)
  write_csv(df, paste0("data/KIT-frozen_baseline/", date, "-KIT-frozen_baseline.csv"))
}

# Combine all files into one dataframe
df <- data.frame()
for (date in dates) {
  df_temp <- read_csv(paste0("data/KIT-frozen_baseline/", date, "-KIT-frozen_baseline.csv"),
    show_col_types = FALSE, progress = FALSE
  )
  df <- bind_rows(df, df_temp)
}
df$model <- "KIT-frozen_baseline"

write_csv(df, "data/submissions_KIT-frozen_baseline.csv.gz")
