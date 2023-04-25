library(tidyverse)
library(jsonlite)
library(runner)

load_reporting_triangle <- function(as_of) {
  # retrieve all commits on the given date
  commits <- fromJSON(
    paste0(
      "https://api.github.com/repos/KITmetricslab/hospitalization-nowcast-hub/commits?path=data-truth/COVID-19/COVID-19_hospitalizations.csv",
      "&since=", as.Date(as_of) - 1,
      "&until=", as_of
    ),
    simplifyDataFrame = TRUE, flatten = TRUE
  )

  # get sha of latest commit on the given date
  sha <- commits %>%
    mutate(date = as.Date(commit.author.date)) %>%
    filter(date == as_of) %>%
    filter(commit.author.date == max(commit.author.date)) %>%
    pull(sha)

  # load the corresponding data
  read_csv(paste0(
    "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/",
    sha,
    "/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv"
  ), show_col_types = FALSE)
}


load_truth <- function(as_of, load_precomputed = TRUE) {
  # check if file already exists
  filename <- paste0("data/truth_", as_of, ".csv")

  if (load_precomputed & file.exists(filename)) {
    print(paste0(
      "Truth data has already been computed. Loading: ", filename, ".",
      "If you want to recompute it, set `load_precomputed = FALSE`."
    ))
    read_csv(filename, show_col_types = FALSE)
  } else {
    df <- load_reporting_triangle(as_of)

    # rowwise sum to aggregate the corrections across all delays
    df <- df %>%
      mutate(value = rowSums(across(starts_with("value")), na.rm = TRUE))

    # compute 7-day rolling sum within each stratum
    df %>%
      group_by(location, age_group) %>%
      mutate(truth = sum_run(value, 7, na_pad = TRUE)) %>%
      select(date, location, age_group, truth) %>%
      drop_na()
  }
}

# df_truth <- load_truth("2022-08-08")
# write_csv(df_truth, "data/truth_2022-08-08.csv")


### Functions to load "frozen" truth values

make_frozen_indices <- function(lead_time = 0) {
  m <- matrix(NA, nrow = 7, ncol = 7 + lead_time)
  m <- col(m) <= row(m) + lead_time
  m[7:1, ]
}

frozen_sum <- function(df, indices_frozen) {
  if (nrow(df) != 7) {
    return(NA)
  } else {
    values <- df %>%
      ungroup() %>%
      select(any_of(paste0("value_", 0:(ncol(indices_frozen) - 1), "d")))

    sum(values[indices_frozen[, 1:ncol(values)]])
  }
}

load_frozen_truth <- function(lead_time = 0, start_date = "2021-11-01", load_precomputed = TRUE) {
  # check if file already exists
  filename <- paste0("data/truth_frozen/truth_frozen_", lead_time, "d.csv")
  if (load_precomputed & start_date == "2021-11-01" & file.exists(filename)) {
    print(paste0(
      "Frozen truth has already been computed. Loading: ", filename, ".",
      "If you want to recompute it, set `load_precomputed = FALSE`."
    ))
    df <- read_csv(filename, show_col_types = FALSE)
  } else {
    indices_frozen <- make_frozen_indices(lead_time)

    df <- read_csv("../hospitalization-nowcast-hub/data-truth/COVID-19/COVID-19_hospitalizations.csv",
      show_col_types = FALSE
    ) %>%
      filter(date >= start_date) %>%
      rename(value_81d = `value_>80d`)

    df <- df %>%
      group_by(location, age_group) %>%
      run_by(idx = "date", k = "7 days") %>%
      mutate(frozen_value = runner(x = ., f = function(x) {
        frozen_sum(x, indices_frozen)
      })) %>%
      select(c(date, location, age_group, frozen_value)) %>%
      drop_na()
  }
}

# truth_frozen <- load_frozen_truth(0, "2021-11-01")
# truth_frozen2 <- load_frozen_truth(2, "2021-11-01")
# truth_frozen14 <- load_frozen_truth(14, "2021-11-01")
