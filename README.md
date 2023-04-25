# Collaborative nowcasting of COVID-19 hospitalization incidences in Germany

Preprint: https://www.medrxiv.org/content/10.1101/2023.04.17.23288668v1

# Figures
- Figure 1: [`01_plot_nowcast_example.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/01_plot_nowcast_example.R)
- Figure 2: [`02-13-14_plot_fraction.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/02-13-14_plot_fraction.R)
- Figure 3: [`03_plot_ensemble_intuition.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/03_plot_ensemble_intuition.R)
- Figure 4, 5: [`04-05_plot_nowcasts_by_horizon.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/04-05_plot_nowcasts_by_horizon.R)
- Figure 6: [`06_plot_wis.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/06_plot_wis.R)
- Figure 7: [`07_plot_coverage.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/07_plot_coverage.R)
- Figure 8: [`08_plot_scores_7d.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/08_plot_scores_7d.R)
- Figure 9a: [`09a_plot_saxony.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/09a_plot_saxony.R)
- Figure 9b: [`09b_plot_bremen.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/09b_plot_bremen.R)
- Figure 9c, 9d: [`09cd_plot_easter.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/09cd_plot_easter.R)
- Figure 10: [`10_plot_scores_updated.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/10_plot_scores_updated.R)
- Figure 11: [`11-17_plot_scores_by_eval_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/11-17_plot_scores_by_eval_date.R)
- Figure 12: [`12_plot_scores_40d.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/12_plot_scores_40d.R)
- Figure 13, 14: [`02-13-14_plot_fraction.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/02-13-14_plot_fraction.R)
- Figure 15, 16: [`15-16_plot_scores.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/15-16_plot_scores.R)
- Figure 17: [`11-17_plot_scores_by_eval_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/11-17_plot_scores_by_eval_date.R)
- Figure 18: [`18_plot_ranks.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/18_plot_ranks.R)
- Figure 19: [`19_plot_scores_by_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/19_plot_scores_by_date.R)
- Figure 20: [`20_plot_scores_100k.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/20_plot_scores_100k.R)

# Tables
- Table 6:  [`pairwise_comparison.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation/blob/main/code/score_computation/pairwise_comparison.R)

# Requirements
We use [`renv`](https://rstudio.github.io/renv/index.html) to manage package versions and dependencies. After cloning the repository, please open the `.Rproj` file
using RStudio and run 
``` r
renv::restore()
``` 
to install all required packages into the project library (which is isolated from other R libraries on your system). 

For further information, please refer to https://rstudio.github.io/renv/articles/renv.html, especially to the section [Collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html).
