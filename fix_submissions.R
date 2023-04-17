library(tidyverse)
library(rriskDistributions)

### EPIFORECASTS

interpolate_quantiles <- function (p = 0.1, p_obs = c(0.025, 0.25), q_obs){
  if(q_obs[1] == q_obs[2]){
    q_obs[1]
  } else {
    res <- suppressMessages(
      get.norm.par(p = p_obs, q = sort(q_obs), show.output = FALSE, plot = FALSE)
    )
    qnorm(p, mean = res["mean"], sd = res["sd"])
  }
}


fix_epiforecasts <- function(df) {
  df1 <- df %>% 
    filter(!(model == "Epiforecasts" & type == "quantile"))
  
  df2 <- df %>% 
    filter(model == "Epiforecasts",
           type == "quantile")
  
  temp <- df2 %>% 
    group_by(location, age_group, target_end_date, forecast_date, target) %>% 
    mutate(incomplete = all(quantile != 0.1))
  
  temp1 <- temp %>% 
    filter(incomplete == FALSE) 
  
  temp2 <- temp %>% 
    filter(incomplete == TRUE) 
  
  temp2 <- temp2 %>% 
    pivot_wider(names_from = quantile, values_from = value) %>% 
    mutate(`0.1` = interpolate_quantiles(0.1, c(0.025, 0.25), c(`0.025`, `0.25`))) %>% 
    pivot_longer(cols = c("0.025", "0.1", "0.25", "0.5", "0.75", "0.9", "0.975"),
                 names_to = "quantile") %>% 
    mutate(quantile = as.numeric(quantile))
  
  df2 <- bind_rows(temp1, temp2) %>% 
    select(-incomplete)
  
  bind_rows(df1, df2)
}


### RKI
# Some 0 day ahead nowcasts are unrealisticly large (>1 billion).
# We replace them with the respective -1 day ahead nowcasts.

fix_RKI <- function(df) {
  df_error <- df %>%
    filter(
      model == "RKI",
      value > 1000000 | is.na(value)
    ) %>%
    select(c(location, age_group, forecast_date, type, quantile))
  
  df_replacement <- df %>%
    filter(
      model == "RKI",
      target %in% c("0 day ahead inc hosp", "-1 day ahead inc hosp")
    )
  
  df_fixed <- df_error %>%
    left_join(df_replacement) %>%
    group_by(location, age_group, forecast_date, type, quantile, pathogen, model, retrospective) %>%
    summarize(
      target_end_date = max(target_end_date),
      target = max(target),
      value = min(value, na.rm = TRUE)
    )
  
  df <- df %>%
    filter(!(model == "RKI" & (value > 1000000 | is.na(value))))
  df <- bind_rows(df, df_fixed)
  
  return(df)
}


### ILM

fix_ILM <- function(df) {
  # Identify missing dates
  df_ILM <- read_csv("data/submission_check/ILM-missing.csv", show_col_types = FALSE)
  missing_dates <- as.character(unique(df_ILM$forecast_date))
  
  # Path to updated ILM model
  PATH_ILM <- "../hospitalization-nowcast-hub/data-processed_updated_models/ILM-prop/"
  
  # Updated files to load
  files <- paste0(missing_dates, "-ILM-prop.csv")
  
  # Load all submissions into one dataframe in long format
  df_new <- data.frame()
  for (f in files) {
    df_temp <- read_csv(paste0(PATH_ILM, f),
                        show_col_types = FALSE, progress = FALSE
    )
    df_temp$model <- "ILM"
    df_temp$retrospective <- TRUE
    df_new <- bind_rows(df_new, df_temp)
  }
  
  # Fill missing entries
  df_ILM <- df_ILM %>% 
    left_join(df_new)
  
  bind_rows(df, df_ILM)
}


### LMU

fix_LMU <- function(df) {
  # Identify missing dates
  df_LMU <- read_csv("data/submission_check/LMU-missing.csv", show_col_types = FALSE)
  missing_dates <- as.character(unique(df_LMU$forecast_date))
  
  # Path to updated LMU model
  PATH_LMU <- "../hospitalization-nowcast-hub/data-processed_updated_models/LMU_StaBLab-GAM_nowcast/"
  
  # Updated files to load
  files <- paste0(missing_dates, "-LMU_StaBLab-GAM_nowcast.csv")
  
  # Load all submissions into one dataframe in long format
  df_new <- data.frame()
  for (f in files) {
    df_temp <- read_csv(paste0(PATH_LMU, f),
                        show_col_types = FALSE, progress = FALSE
    )
    df_temp$model <- "LMU"
    df_temp$retrospective <- TRUE
    df_new <- bind_rows(df_new, df_temp)
  }
  
  # Fill missing entries
  df_LMU <- df_LMU %>% 
    left_join(df_new)
  
  bind_rows(df, df_LMU)
}
