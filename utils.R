source("fix_submissions.R")
source("load_truth.R")

Sys.setlocale("LC_ALL", "C")

SHORT_NAMES <- c(
  "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
  "LMU", "MeanEnsemble", "MedianEnsemble",
  "RIVM", "RKI", "SU", "SZ"
)

MODELS <- c(
  "Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI",
  "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
)

ALL_MODELS <- c(
  "Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI",
  "SU", "SZ", "KIT-frozen_baseline", "MeanEnsemble", "MedianEnsemble"
)

ALL_MODELS_UPDATED <- c(
  "ILM", "ILM (updated)", "KIT", "KIT (updated)", "LMU", "LMU (updated)", "RKI", "RKI (updated)" 
)

UPDATED_MODELS <- c("ILM", "KIT", "LMU", "RKI")

MODEL_COLORS <- setNames(
  c("#B30000", "#E69F00", "#999999", "#56B4E9", "#F0E442", "#009E73", "#60D1B3", "#80471C", "#3C4AAD", "#CC79A7", "#000000"),
  c("Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT", "LMU", "MeanEnsemble", "MedianEnsemble", "RIVM", "RKI", "SU", "SZ")
)

UPDATED_COLORS <- setNames(
  c("#56B4E9", "#3c7da3", "#F0E442", "#a89f2e", "#E69F00", "#a16f00", "#3C4AAD", "#2a3379"),
  c("KIT", "KIT (updated)", "LMU", "LMU (updated)", "ILM", "ILM (updated)", "RKI", "RKI (updated)")
)

TITLES <- setNames(
  c("National level", "States", "Age groups"),
  c("national", "states", "age")
)

METRICS <- setNames(
  c("absolute error", "squared error", "WIS"),
  c("median", "mean", "quantile")
)


load_data <- function(add_baseline = TRUE, add_median = FALSE, shorten_names = TRUE, fix_data = TRUE,
                      add_truth = TRUE, exclude_missing = TRUE, eval_date = "2022-08-08", per_100k = FALSE) {
  df <- read_csv("data/submissions.csv.gz", show_col_types = FALSE)

  # Add baseline
  if (add_baseline) {
    df_baseline <- read_csv("data/submissions_KIT-frozen_baseline.csv.gz", show_col_types = FALSE) %>%
      mutate(retrospective = FALSE)
    df <- bind_rows(df, df_baseline)
  }

  if (shorten_names) {
    df$model <- factor(df$model,
      levels = sort(unique(df$model)),
      labels = if (add_baseline) SHORT_NAMES else SHORT_NAMES[-3]
    )
  }

  # Fix incomplete and erroneus nowcasts
  if (fix_data) {
    df <- fix_RKI(df)
    df <- fix_epiforecasts(df)
    df <- fix_ILM(df)
    df <- fix_LMU(df)
  }
  
  # Exclude missing entries from all submissions 
  if (exclude_missing) {
    files <- c("Epiforecasts-missing.csv", "MeanEnsemble-missing.csv", "MedianEnsemble-missing.csv", 
               "SZ-missing.csv")
    
    df_missing <- data.frame()
    for (f in files) {
      df_temp <- read_csv(paste0("data/submission_check/fixed/", f),
                          show_col_types = FALSE, progress = FALSE)
      df_missing <- bind_rows(df_missing, df_temp)
    }
    
    df <- anti_join(df, df_missing)
  }

  # Add median separately
  if (add_median) {
    df_median <- df %>%
      filter(quantile == 0.5) %>%
      mutate(type = "median")
    df <- bind_rows(df, df_median)
  }

  # Add truth
  if (add_truth) {
    df_truth <- load_truth(as_of = eval_date)

    df <- df %>%
      left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
  }
  
  # Standardize per 100k population
  if (per_100k) {
    df_population <- read_csv("../hospitalization-nowcast-hub/nowcast_viz_de/plot_data/population_sizes.csv")  %>% 
      select(-"...1")
    
    df <- df %>% 
      left_join(df_population) %>% 
      mutate(value = value*100000/population,
             truth = truth*100000/population)
  }

  return(df)
}


filter_scores <- function(df, type = "quantile", level = "national", 
                          by_horizon = FALSE, average = TRUE) {
  df <- df %>%
    filter(type == !!type)
  
  if (level == "national") {
    df <- df %>%
      filter(
        location == "DE",
        age_group == "00+"
      )
  } else if (level == "states") {
    df <- df %>%
      filter(
        location != "DE",
        model != "ILM"
      )
  } else if (level == "age") {
    df <- df %>%
      filter(
        location == "DE",
        age_group != "00+",
        model != "RKI"
      )
  }


  if (by_horizon) {
    df <- df %>%
      mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>%
      arrange(model, location, age_group, horizon)

    if (average) {
      df <- df %>%
        group_by(model, target, horizon) %>%
        summarize(score = mean(score), .groups = "drop") %>%
        arrange(model, horizon)
    }
  } else {
    if (average) {
      df <- df %>%
        group_by(model) %>%
        summarize(score = mean(score), .groups = "drop")
    } else {
      df <- df %>%
        group_by(model, location, age_group) %>%
        summarize(score = mean(score), .groups = "drop")
    }
  }

  return(df)
}


load_scores <- function(short_horizons = FALSE, per_100k = FALSE, updated_models = FALSE, 
                        truth_40d = FALSE, load_baseline = TRUE) {
  df <- read_csv(paste0("data/scores", ifelse(per_100k, "_100k", ""), 
                        ifelse(updated_models, "_updated", ""), ifelse(truth_40d, "_40d", ""), 
                        ".csv.gz"),
                 show_col_types = FALSE)

  if (!load_baseline) {
    df <- df %>%
      filter(model != "KIT-frozen_baseline")
  }
  
  if (short_horizons) {
    df <- df %>%
      filter(target %in% paste(0:7 * -1, "day ahead inc hosp"))
  }
  
  
  # Reorder models (ensembles at the end)
  if (updated_models) {
    df <- df %>% mutate(model = fct_relevel(model, UPDATED_MODELS))
  } else {
    df <- df %>%
      mutate(model = fct_relevel(model, if (load_baseline) ALL_MODELS else MODELS))
  }

  
  return(df)
}


filter_data <- function(df, model, type, level = "national", short_horizons = FALSE) {
  if (!missing(type)) {
    df <- df %>% filter(type == !!type)
  }
  
  if (!missing(model)) {
    df <- df %>% filter(model == !!model)
  }
  
  if (short_horizons) {
    df <- df %>% filter(target %in% paste(0:7 * -1, "day ahead inc hosp"))
  }
  
  if (level == "national") {
    df <- df %>%
      filter(
        location == "DE",
        age_group == "00+"
      )
  } else if (level == "states") {
    df <- df %>%
      filter(
        location != "DE",
        model != "ILM"
      )
  } else if (level == "age") {
    df <- df %>%
      filter(
        location == "DE",
        age_group != "00+",
        model != "RKI"
      )
  }
  return(df)
}


load_nowcast <- function(model, date, location = "DE", age_group = "00+") {
  read_csv(paste0("../hospitalization-nowcast-hub/data-processed/", model, "/", date, "-", model, ".csv"),
           show_col_types = FALSE
  ) %>%
    filter(
      location == {{ location }},
      age_group == {{ age_group }},
      type == "quantile"
    ) %>%
    pivot_wider(names_from = quantile, names_prefix = "quantile_")
}


# Quantile score
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
}

score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    return((prediction - observation)^2)
  } else if (type == "median") {
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    return(qs(prediction, observation, quantile))
  }
}

