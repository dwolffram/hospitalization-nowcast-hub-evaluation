library(tidyverse)

START_DATE <- "2021-11-22"
END_DATE <- "2022-04-29"

### PROSPECTIVE SUBMISSIONS

PATH <- "../hospitalization-nowcast-hub/data-processed/"
files <- list.files(PATH, pattern = "*.csv", recursive = TRUE)

df_files <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )


### RETROSPECTIVE SUBMISSIONS

PATH_RETRO <- "../hospitalization-nowcast-hub/data-processed_retrospective/"
files <- list.files(PATH_RETRO, pattern = "*.csv", recursive = TRUE)

df_files2 <- data.frame(path = files) %>%
  mutate(model = str_split(path, "/", simplify = TRUE)[, 1]) %>%
  mutate(forecast_date = str_extract(path, "\\d{4}-\\d{2}-\\d{2}")) %>%
  filter(
    forecast_date >= START_DATE,
    forecast_date <= END_DATE
  )

df_files2 <- df_files2 %>%
  filter(str_detect(path, "raw", negate = TRUE))


### COMBINE PROSPECTIVE AND RETROSPECTIVE SUBMISSIONS

df_files <- df_files %>%
  mutate(retrospective = FALSE)

df_files2 <- df_files2 %>%
  mutate(retrospective = TRUE)

d <- bind_rows(df_files, df_files2)

d <- d %>%
  group_by(model, forecast_date) %>%
  mutate(duplicated = n() > 1)

# if duplicated (prospective + retrospective) remove prospective submission
d <- d %>%
  filter(!(!retrospective & duplicated))

d <- d %>%
  rowwise() %>%
  mutate(folder = if (retrospective) PATH_RETRO else PATH)

# load all submissions into one dataframe in long format
df <- data.frame()
pb <- txtProgressBar(min = 0, max = nrow(d), style = 3)
for (i in 1:nrow(d)) {
  row <- d[i, ]
  df_temp <- read_csv(paste0(row$folder, row$path),
    show_col_types = FALSE, progress = FALSE
  )
  df_temp$model <- row$model
  df_temp$retrospective <- row$retrospective
  df <- bind_rows(df, df_temp)

  setTxtProgressBar(pb, i)
}

write_csv(df, paste0("data/submissions.csv.gz"))
