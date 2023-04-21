library(patchwork)
library(ggridges)

### SCORES (AE, MSE, QS)

plot_scores <- function(type = "quantile", level = "national",
                        by_horizon = FALSE, relative = FALSE, add_ae = FALSE,
                        short_horizons = FALSE, per_100k = FALSE) {
  df <- load_scores(short_horizons = short_horizons, per_100k = per_100k, load_baseline = TRUE)
  scores <- filter_scores(df, type, level, by_horizon)
  metric <- METRICS[type]
  ylabel <- if (relative) paste("Relative", metric) else paste("Mean", metric)

  if (by_horizon) {
    if (relative) {
      base_scores <- scores %>%
        filter(model == "KIT-frozen_baseline") %>%
        select(-c(model, target)) %>%
        rename(base_score = score)

      scores <- scores %>%
        left_join(base_scores, by = "horizon") %>%
        mutate(score = score / base_score)
    }

    scores <- scores %>%
      filter(model != "KIT-frozen_baseline")

    ggplot(scores, aes(x = horizon, y = score, color = model)) +
      geom_line() + # size = 1
      scale_color_manual(values = MODEL_COLORS) +
      labs(
        x = "Horizon (days)",
        y = ylabel,
        color = "Model"
      ) +
      scale_x_continuous(
        breaks = 0:5 * -5,
        minor_breaks = -28:0
      ) +
      expand_limits(y = 0) +
      theme_bw() +
      theme(legend.position = "none")
  } else {
    if (relative) {
      base_score <- scores %>%
        filter(model == "KIT-frozen_baseline") %>%
        pull(score)
    }

    scores <- scores %>%
      mutate(model = fct_relevel(model, c(
        "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
        "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
      )))

    scores <- scores %>%
      filter(model != "KIT-frozen_baseline") %>%
      mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))

    if (add_ae) {
      scores_ae <- filter_scores(df, "median", level, by_horizon) %>%
        filter(model != "KIT-frozen_baseline") %>%
        mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))
    }

    ggplot() +
      {
        if (add_ae) geom_point(data = scores_ae, aes(x = model, y = score, fill = model), shape = 23)
      } +
      geom_bar(data = scores, aes(x = model, y = score, fill = model), stat = "identity") +
      geom_label(
        data = scores, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
        fill = "white", alpha = 1, hjust = 0.5,
        label.r = unit(0.25, "lines"), size = 5 / .pt,
        label.padding = unit(0.15, "lines")
      ) +
      scale_fill_manual(values = MODEL_COLORS) +
      scale_color_manual(values = MODEL_COLORS) +
      scale_x_discrete(limits = rev, drop = FALSE) +
      labs(
        y = ylabel,
        x = NULL,
        color = "Model"
      ) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      {
        if (relative) {
          scale_y_continuous(
            name = paste("Mean", metric),
            sec.axis = sec_axis(trans = ~ . / base_score, name = paste("Relative", metric))
          )
        }
      }
  }
}


### WIS DECOMPOSITION

plot_wis <- function(level = "national", add_ae = TRUE, short_horizons = FALSE,
                     per_100k = FALSE, updated_models = FALSE, truth_40d = FALSE) {

  # Load the respective file with WIS decomposition
  df <- read_csv(paste0(
    "data/wis_", level, ifelse(short_horizons, "_7d", ""),
    ifelse(per_100k, "_100k", ""), ifelse(truth_40d, "_40d", ""), ".csv"
  ), show_col_types = FALSE)

  # Get score of the baseline model to compute relative axis
  base_score <- df %>%
    filter(model == "KIT-frozen_baseline") %>%
    pull(score)

  # Combine original scores with scores of the updated models
  # (to show all models even when they did not cover states or age groups)
  if (updated_models) {
    df_updated <- read_csv(paste0("data/wis_", level, "_updated.csv"), show_col_types = FALSE) %>%
      mutate(model = paste(model, "(updated)"))

    df <- df %>%
      filter(model %in% UPDATED_MODELS) %>%
      bind_rows(df_updated) %>%
      mutate(
        model = fct_expand(model, ALL_MODELS_UPDATED),
        model = fct_relevel(model, ALL_MODELS_UPDATED)
      )
  } else {
    df <- df %>%
      filter(model %in% MODELS) %>%
      mutate(
        model = fct_expand(model, MODELS),
        model = fct_relevel(model, MODELS)
      )
  }

  scores <- df %>%
    select(-score) %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")

  # Load absolute errors from the respective file
  if (add_ae) {
    df_ae <- load_scores(
      load_baseline = FALSE, short_horizons = short_horizons,
      per_100k = per_100k, updated_models = FALSE, truth_40d = truth_40d
    )

    df_ae <- filter_scores(df_ae, "median", level, by_horizon = FALSE)

    # Combine original scores with scores of the updated models
    if (updated_models) {
      df_ae_updated <- load_scores(
        load_baseline = FALSE, short_horizons = short_horizons,
        per_100k = per_100k, updated_models = TRUE
      )

      df_ae_updated <- filter_scores(df_ae_updated, "median", level, by_horizon = FALSE) %>%
        mutate(model = paste(model, "(updated)"))

      df_ae <- df_ae %>%
        filter(model %in% c("ILM", "KIT", "LMU", "RKI")) %>%
        bind_rows(df_ae_updated)
    }
  }

  ggplot() +
    {
      if (add_ae) {
        geom_point(
          data = df_ae, aes(x = model, y = score, fill = model),
          shape = 23, size = 0.5
        )
      }
    } +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + # so you can't see through bars
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.15, "lines"), # 0.25
      size = 5 / .pt,
      label.padding = unit(0.1, "lines") # 0.15
    ) +
    scale_fill_manual(values = if (updated_models) UPDATED_COLORS else MODEL_COLORS, guide = "none") +
    scale_color_manual(values = if (updated_models) UPDATED_COLORS else MODEL_COLORS, guide = "none") +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    scale_x_discrete(limits = rev, drop = FALSE) +
    scale_y_continuous(
      name = paste0("Mean WIS", ifelse(add_ae, " / AE", "")),
      sec.axis = sec_axis(
        trans = ~ . / base_score,
        name = paste("Relative", "WIS")
      )
    ) +
    labs(
      x = NULL,
      color = "Model",
      alpha = "Decomposition of WIS"
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
}


### COVERAGE ACROSS ALL HORIZONS

plot_coverage <- function(df, level = "national", updated_models = FALSE) {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

  df <- df %>%
    mutate(
      model = fct_expand(model, if (updated_models) ALL_MODELS_UPDATED else MODELS),
      model = fct_relevel(model, if (updated_models) ALL_MODELS_UPDATED else MODELS)
    )

  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )

  coverage_df <- df_wide %>%
    group_by(model) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )

  alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))

  ggplot(coverage_df, aes(x = model)) +
    expand_limits(y = 1) +
    geom_col(aes(y = c95, fill = model, alpha = "95%")) +
    geom_col(aes(y = c50, fill = model, alpha = "50%")) +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      x = NULL,
      y = "Empirical coverage",
      color = "Model",
      alpha = "Prediction \ninterval"
    ) +
    scale_fill_manual(values = if (updated_models) UPDATED_COLORS else MODEL_COLORS) +
    coord_flip() +
    scale_x_discrete(limits = rev, drop = FALSE) +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    theme(legend.position = c(0.9, 0.35), legend.justification = c(1, 1), legend.box.just = "left")
}


### COVERAGE BY HORIZON

plot_coverage_lines <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

  df <- df %>%
    mutate(
      model = fct_expand(model, MODELS),
      model = fct_relevel(model, MODELS)
    )

  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )

  coverage_df <- df_wide %>%
    group_by(model, horizon) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )

  coverage_long <- coverage_df %>%
    pivot_longer(cols = c(c50, c95), names_to = "quantile") %>%
    mutate(
      quantile_label = paste0(str_sub(quantile, 2, 3), "% prediction interval"),
      quantile_level = as.numeric(paste0("0.", str_sub(quantile, 2, 3)))
    )

  nominal_levels <- data.frame(
    quantile_label = c("50% prediction interval", "95% prediction interval"),
    quantile_level = c(0.5, 0.95)
  )


  ggplot(coverage_long, aes(x = horizon, y = value, color = model)) +
    facet_wrap("quantile_label") +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = MODEL_COLORS, limits = force) +
    geom_hline(data = nominal_levels, aes(yintercept = quantile_level), linetype = "dashed") +
    scale_x_continuous(
      breaks = 0:5 * -5,
      minor_breaks = -28:0
    ) +
    labs(
      x = "Horizon (days)",
      y = "Empirical coverage",
      color = "Model",
      title = TITLES[level]
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}


### RANKS

plot_ranks <- function(df, level = "national") {
  temp <- filter_data(df, type = "quantile", level = level)
  
  df_rank <- temp %>%
    group_by(location, age_group, target, target_end_date, model) %>%
    summarize(mean_score = mean(score)) %>%
    group_by(location, age_group, target, target_end_date) %>%
    arrange(model, mean_score) %>%
    mutate(rank = rank(mean_score, ties.method = "min")) %>%
    arrange(target_end_date)
  
  m <- length(unique(df_rank$model))
  
  df_rank <- df_rank %>%
    group_by(model) %>%
    mutate(
      rank = 1 - (rank - 1) / (m - 1),
      meanRank = mean(rank)
    )
  
  df_mean_rank <- df_rank %>%
    group_by(model) %>%
    summarize(rank = mean(rank))
  
  COLORS <- setNames(c("#440154", "#31688e", "#35b779", "#fde725"), 1:4)
  
  ggplot(df_rank, aes(
    x = rank, y = reorder(model, meanRank),
    fill = factor(stat(quantile))
  )) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE,
      bandwidth = 0.075,
      scale = 0.9,
      alpha = 0.8
    ) +
    scale_fill_manual(name = "Quartile", values = alpha(COLORS, 0.6)) +
    # scale_fill_viridis_d(name = "Quartiles", breaks = 1:4, alpha = 0.6) +
    geom_point(data = df_mean_rank, aes(x = rank, y = model), inherit.aes = FALSE, shape = 3) +
    xlab("Standardized rank") +
    ylab("Model") +
    xlim(c(0, 1)) +
    theme_bw()
}


### RELATIVE WIS BY NOWCAST DATE

plot_scores_by_date <- function(df, level = "national", relative = TRUE) {
  df_temp <- df %>%
    filter(type == "quantile")
  
  df_temp <- filter_data(df_temp, level = level) %>%
    group_by(model, forecast_date) %>%
    summarize(score = mean(score))
  
  if (relative) {
    base_score <- df_temp %>%
      filter(model == "KIT-frozen_baseline") %>%
      rename(base_score = score) %>%
      ungroup() %>%
      select(-model)
    
    df_temp <- left_join(df_temp, base_score, by = c("forecast_date")) %>%
      mutate(score = score / base_score)
  }
  
  df_temp <- df_temp %>% filter(model != "KIT-frozen_baseline")
  
  ggplot(df_temp, aes(x = forecast_date, y = score, color = model)) +
    geom_line(size = 0.4) +
    scale_color_manual(breaks = MODELS, values = MODEL_COLORS) +
    theme_bw() +
    labs(
      x = NULL,
      y = ifelse(relative, "Relative WIS", "Mean WIS"),
      color = "Model",
      title = TITLES[level]
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
}


### MEAN WIS BY EVALUATION DATE

plot_scores_by_eval_date <- function(df, level = "national") {
  title <- TITLES[level]
  
  df_temp <- df %>%
    filter(level == !!level)
  
  ggplot(df_temp, aes(x = eval_date, y = score, color = model)) +
    geom_vline(xintercept = as.Date("2022-08-08"), color = "darkgray", linetype = "solid") +
    annotate(
      geom = "label", x = as.Date("2022-08-08"), y = 0.05 * max(df_temp$score),
      label = "8 August 2022", color = "darkgray", size = 6 / .pt,
      label.padding = unit(0.1, "lines")
    ) +
    geom_line() +
    scale_x_date(date_breaks = "2 months", minor_breaks = "1 month", date_labels = "%b %Y") +
    expand_limits(y = 0) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(breaks = MODELS, values = MODEL_COLORS) +
    labs(
      x = "Evaluation date",
      y = "Mean WIS",
      color = "Model",
      title = title
    )
}