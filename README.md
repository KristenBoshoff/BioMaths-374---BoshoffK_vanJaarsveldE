# Logistic regression model of multiple births in sheep (BioMaths 374)

This repository contains the data, R code and figures for our BioMaths 374 group project:
**A Logistic Regression Model to Estimate the Probability of Multiple Births in Sheep**.

The model uses FecB genotype (++, B+, BB) and parity (1 to 3) to estimate the probability that a
ewe gives birth to multiples (litter size of 2 or more). It is fitted on a training set (70% of ewes)
and tested on the remaining 30%.

## Folder structure
- `data/DATA_EDITED2.xlsx`: the data, with the training/testing assignment
- `R/`: the R scripts, numbered in the order they must be run
- `figures/`: figures created by the scripts

## Scripts
| Script | What it does |
|---|---|
| `01_import_and_model.R` | Imports the training ewes, builds the data and fits the logistic regression |
| `02_test_set.R` | Builds the testing set |
| `03_validation.R` | Predicts the testing set; AUC, Brier score, confusion matrix |
| `04_fig_genotype_litter.R` | Figure: litter size by genotype |
| `05_fig_parity_litter.R` | Figure: litter size by parity |
| `06_fig_predictions_vs_observed.R` | Figure: model predictions vs observed testing data |
| `07_fig_predicted_probability_ci.R` | Figure: predicted probabilities with 95% CI |
| `08_fig_logistic_scurve.R` | Figure: logistic S-curve |
| `main.R` | Runs all scripts in order |

## How to run
1. Click **Code > Download ZIP** and unzip the folder.
2. Open R or RStudio and set the working directory to the unzipped folder (the one containing `R/` and `data/`):
   `setwd("path/to/unzipped/folder")`
3. Install the packages (once):
   `install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "pROC"))`
4. Run everything: `source("R/main.R")` (or run the scripts `01` to `08` one by one, in order).

The figures are saved in `figures/`.

## Expected results
Fitted coefficients (training set, 922 lambings):
intercept -0.167, B+ 1.419, BB 1.834, parity 2 0.174, parity 3 0.735.
Testing set (392 lambings): AUC 0.641, Brier score 0.1588.

## Data source
Data for 890 Small Tail Han ewes (FecB genotype and litter size up to the third parity) were obtained from the supplementary tables of Wang, X. et al. Effects of FecB mutation on estrus, ovulation, and endocrine characteristics in Small Tail Han sheep. *Front. Vet. Sci.* **8**, 709737 (2021). https://doi.org/10.3389/fvets.2021.709737

## Authors
[Author 1] and [Author 2], BioMaths 374, Stellenbosch University.
