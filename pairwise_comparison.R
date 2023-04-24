### Pairwise comparisons to check retrospective nowcasts did not have too much of an impact on relative performance.

library(readr)

# Read in scores:
scores <- read_csv("data/scores.csv.gz")

# restrict to quantile scores 
scores <- subset(scores, type == "quantile")
# aggregate over quantiles to get WIS:
scores <- aggregate(data = scores, 
                    score ~ location + age_group + model + forecast_date + target_end_date + model + retrospective,
                    FUN = mean)

# all present in the same number apart from ILM which did not do states and RKI which did not do age groups
# table(scores$model) 

# get model names:
models <- unique(scores$model)

# compute averages:

# Subset to the three relevant groups of targets:
# national level
scores_DE <- subset(scores, location == "DE" & age_group == "00+")
# states
scores_states <- subset(scores, location != "DE" & age_group == "00+")
# age groups
scores_age_groups <- subset(scores, location == "DE" & age_group != "00+")

# compute averages and normalize to baseline:
average_scores_DE <- aggregate(data = scores_DE, score ~ model, FUN = mean)
average_scores_DE$rescaled_relative_WIS_imputed <- average_scores_DE$score/
  average_scores_DE$score[average_scores_DE$model == "KIT-frozen_baseline"]

average_scores_states <- aggregate(data = scores_states, score ~ model, FUN = mean)
average_scores_states$rescaled_relative_WIS_imputed <- average_scores_states$score/
  average_scores_states$score[average_scores_states$model == "KIT-frozen_baseline"]

average_scores_age_groups <- aggregate(data = scores_age_groups, score ~ model, FUN = mean)
average_scores_age_groups$rescaled_relative_WIS_imputed <- average_scores_age_groups$score/
  average_scores_age_groups$score[average_scores_DE$model == "KIT-frozen_baseline"]

# remove retrospective ones:
scores_prosp <- subset(scores, retrospective == FALSE)

# Subset to the three relevant groups of targets:
# national level
scores_DE_prosp <- subset(scores_prosp, location == "DE" & age_group == "00+")
# states
scores_states_prosp <- subset(scores_prosp, location != "DE" & age_group == "00+")
# age groups
scores_age_groups_prosp <- subset(scores_prosp, location == "DE" & age_group != "00+")

# remove main file (which is large)
rm(scores)

# function for comparison of one pair of models:
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores))){
  # apply subset:
  scores <- scores[subset, ]
  
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("forecast_date", "location", "age_group"),
               all.x = FALSE, all.y = FALSE)
  
  # compute ratio:
  ratio <- sum(sub$score.x) / sum(sub$score.y)
  
  return(list(ratio = ratio, mx = mx, my = my))
}

# matrices to store ratios:
results_ratio_DE <- results_ratio_states <- results_ratio_age_groups <- 
  matrix(ncol = length(models),
         nrow = length(models),
         dimnames = list(models, models))

# run for DE:
for(mx in seq_along(models)){
  cat("Starting", mx, "...\n")
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores_DE_prosp, mx = models[mx], my = models[my])
    results_ratio_DE[mx, my] <- pwc$ratio
    results_ratio_DE[my, mx] <- 1/pwc$ratio
  }
}

geom_mean_ratios_DE <- exp(rowMeans(log(results_ratio_DE), na.rm = TRUE))

# re-scale to KIT-frozen_baseline:
scaled_ratios_DE <- geom_mean_ratios_DE/geom_mean_ratios_DE["KIT-frozen_baseline"]

result_DE <- data.frame(model = names(geom_mean_ratios_DE),
                        relative_WIS = geom_mean_ratios_DE,
                        rescaled_relative_WIS = scaled_ratios_DE)
result_DE <- merge(result_DE, average_scores_DE[, -2], by = "model")


write.csv(results_ratio_DE, file = "data/matrix_DE.csv")
write.csv(result_DE, file = "data/pairwise_comparison/pairwise_comparison_DE.csv")

# result_DE <- read.csv("data/pairwise_comparison/pairwise_comparison_DE.csv")
# result_DE$X <- NULL

rm(scores_DE_prosp)

# run for states:
for(mx in seq_along(models)){
  cat("Starting", mx, "...\n")
  for(my in 1:mx){
    cat("   ", my, "\n")
    pwc <- pairwise_comparison(scores = scores_states_prosp, mx = models[mx], my = models[my])
    results_ratio_states[mx, my] <- pwc$ratio
    results_ratio_states[my, mx] <- 1/pwc$ratio
  }
}

geom_mean_ratios_states <- exp(rowMeans(log(results_ratio_states), na.rm = TRUE))

# re-scale to KIT-frozen_baseline:
scaled_ratios_states <- geom_mean_ratios_states/geom_mean_ratios_states["KIT-frozen_baseline"]


result_states <- data.frame(model = names(geom_mean_ratios_states),
                        relative_WIS = geom_mean_ratios_states,
                        rescaled_relative_WIS = scaled_ratios_states)
result_states <- merge(result_states, average_scores_states[, -2], by = "model")


write.csv(results_ratio_states, file = "data/matrix_states.csv")
write.csv(result_states, file = "data/pairwise_comparison/pairwise_comparison_states.csv")

# result_states <- read.csv("data/pairwise_comparison/pairwise_comparison_states.csv")
# result_states$X <- NULL

# run for age groups:
for(mx in seq_along(models)){
  cat("Starting", mx, "...\n")
  for(my in 1:mx){
    cat("   ", my, "\n")
    pwc <- pairwise_comparison(scores = scores_age_groups_prosp, mx = models[mx], my = models[my])
    results_ratio_age_groups[mx, my] <- pwc$ratio
    results_ratio_age_groups[my, mx] <- 1/pwc$ratio
  }
}

geom_mean_ratios_age_groups <- exp(rowMeans(log(results_ratio_age_groups), na.rm = TRUE))

# re-scale to KIT-frozen_baseline:
scaled_ratios_age_groups <- geom_mean_ratios_age_groups/geom_mean_ratios_age_groups["KIT-frozen_baseline"]

result_age_groups <- data.frame(model = names(geom_mean_ratios_age_groups),
                            relative_WIS = geom_mean_ratios_age_groups,
                            rescaled_relative_WIS = scaled_ratios_age_groups)
result_age_groups <- merge(result_age_groups, average_scores_age_groups[, -2], by = "model")

write.csv(results_ratio_age_groups, file = "data/pairwise_comparison/matrix_age_groups.csv")
write.csv(result_age_groups, file = "data/pairwise_comparison/pairwise_comparison_age_groups.csv")

# result_age_groups <- read.csv("data/pairwise_comparison/pairwise_comparison_age_groups.csv")
# result_age_groups$X <- NULL

# create a table for the LaTeX document:
library(xtable)
result_age_groups

to_print <- result_DE[, -2]
to_print <- merge(to_print, result_states[, -2], by = "model", all.x = TRUE)
to_print <- merge(to_print, result_age_groups[, -2], by = "model", all.x = TRUE)
colnames(to_print) <- NULL

print(xtable(to_print, digits = 4), only.contents = TRUE, 
      include.rownames = FALSE, comment = FALSE,
      file = "data/pairwise_comparison/table_pairwise_comparison.tex")
