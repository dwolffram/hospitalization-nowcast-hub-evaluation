# Collaborative nowcasting of COVID-19 hospitalization incidences in Germany

Paper: [https://doi.org/10.1371/journal.pcbi.1011394](https://doi.org/10.1371/journal.pcbi.1011394)

# Figures
- Figure 1: [`01_plot_nowcast_example.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/01_plot_nowcast_example.R)
- Figure 2: [`02_reference_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/02_reference_date.R)
- Figure 3: [`03-A01-A02_plot_fraction.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/03-A01-A02_plot_fraction.R)
- Figure 4: [`04_plot_ensemble_intuition.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/04_plot_ensemble_intuition.R)
- Figure 5, 6: [`05-06_plot_nowcasts_by_horizon.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/05-06_plot_nowcasts_by_horizon.R)
- Figure 7: [`07_plot_wis.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/07_plot_wis.R)
- Figure 8: [`08_plot_coverage.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/08_plot_coverage.R)
- Figure 9: [`09_plot_scores_7d.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/09_plot_scores_7d.R)
- Figure 10: [`10_issues.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/10_issues.R)
- Figure 11: [`11_plot_scores_updated.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/11_plot_scores_updated.R)
- Figure 12: [`12-A06_plot_scores_by_eval_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/12-A06_plot_scores_by_eval_date.R)
- Figure 13: [`13_plot_scores_40d.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/13_plot_scores_40d.R)

# Supplementary figures
- Figure A1, A2: [`03-A01-A02_plot_fraction.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/03-A01-A02_plot_fraction.R)
- Figure A3: [`A03_40d_explanation.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A03_40d_explanation.R)
- Figure A4, A5: [`A04-A05_plot_scores.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A04-A05_plot_scores.R)
- Figure A6: [`12-A06_plot_scores_by_eval_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/12-A06_plot_scores_by_eval_date.R)
- Figure A7: [`A07_plot_ranks.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A07_plot_ranks.R)
- Figure A8: [`A08_wis_weekday_effect.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A08_wis_weekday_effect.R)
- Figure A9, A10: [`A09-A10_weekday_effect.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A09-A10_weekday_effect.R)
- Figure A11: [`A11_plot_scores_by_date.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A11_plot_scores_by_date.R)
- Figure A12: [`A12_plot_scores_100k.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation-new/blob/main/code/visualization/A12_plot_scores_100k.R)



# Tables
- Table A5:  [`pairwise_comparison.R`](https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation/blob/main/code/score_computation/pairwise_comparison.R)

# Requirements
We use [`renv`](https://rstudio.github.io/renv/index.html) to manage package versions and dependencies. After cloning the repository, please open the `.Rproj` file
using RStudio and run 
``` r
renv::restore()
``` 
to install all required packages into the project library (which is isolated from other R libraries on your system). 

For further information, please refer to https://rstudio.github.io/renv/articles/renv.html, especially to the section [Collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html).
