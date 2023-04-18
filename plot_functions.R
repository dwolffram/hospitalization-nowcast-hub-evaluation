library(patchwork)

ALL_MODELS <- c(
  "Epiforecasts", "ILM", "KIT", "LMU", "RIVM", "RKI",
  "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
)

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

plot_wis <- function(level = "national", add_ae = TRUE, short_horizons = FALSE, per_100k = FALSE) {
  df <- read_csv(paste0(
    "data/wis_", level, ifelse(short_horizons, "_7d", ""),
    ifelse(per_100k, "_100k", ""), ".csv"
  ), show_col_types = FALSE)

  df <- df %>%
    mutate(model = fct_relevel(model, c(
      "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    )))

  base_score <- df %>%
    filter(model == "KIT-frozen_baseline") %>%
    pull(score)

  df <- df %>%
    filter(model != "KIT-frozen_baseline") %>%
    mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))

  scores <- df %>%
    select(-score) %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")

  if (add_ae) {
    df_ae <- load_scores(load_baseline = FALSE, short_horizons = short_horizons, per_100k = per_100k)
    df_ae <- df_ae %>%
      mutate(model = fct_relevel(model, c(
        "Epiforecasts", "ILM", "KIT-frozen_baseline", "KIT",
        "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
      )))
    df_ae <- filter_scores(df_ae, "median", level, by_horizon = FALSE) %>%
      filter(model != "KIT-frozen_baseline") %>%
      mutate(model = fct_drop(model, only = "KIT-frozen_baseline"))
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
    scale_fill_manual(values = MODEL_COLORS, guide = "none") +
    scale_color_manual(values = MODEL_COLORS, guide = "none") +
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

plot_coverage <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

  df <- df %>%
    mutate(
      model = fct_expand(model, ALL_MODELS),
      model = fct_relevel(model, ALL_MODELS)
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
    scale_fill_manual(values = MODEL_COLORS) +
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
    mutate(model = fct_relevel(model, c(
      "Epiforecasts", "ILM", "KIT",
      "LMU", "RIVM", "RKI", "SU", "SZ", "MeanEnsemble", "MedianEnsemble"
    )))

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
