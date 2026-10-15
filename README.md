# Logistic regression model for multiple births in sheep

Code and data for the BioMaths 374 project **"A Logistic Regression Model to Estimate the
Probability of Multiple Births in Sheep"**.

The model estimates the probability that a lambing is a multiple birth (litter size >= 2) from the
ewe's **FecB (Booroola) genotype** (`++`, `B+`, `BB`) and her **parity** (1, 2, 3). Both predictors
are coded with dummy (indicator) variables (reference: `++`, parity 1). The coefficients are
estimated by maximum likelihood with `glm()` on a **70% training set of ewes** and the fitted model
is evaluated on the other **30% of ewes**.

## Repository layout

| Path | Content |
|---|---|
| `R/sheep_logistic_traintest.R` | The complete, annotated analysis script (one file, runs top to bottom) |
| `data/DATA_EDITED2.xlsx` | Ewe data: sheets `++`, `B+`, `BB`, each with a training block and a testing block of columns |
| `figures/` | Figures written by the script (PNG) |
| `output/` | Result tables written by the script (CSV) and the full console output |

## How to run

Requirements: R (>= 4.1). The script installs any missing packages (`readxl`, `dplyr`, `tidyr`,
`ggplot2`, `car`, `lme4`, `emmeans`) automatically.

```bash
git clone <this repository>
cd <repository folder>
Rscript R/sheep_logistic_traintest.R
```

In RStudio: open `R/sheep_logistic_traintest.R` and click **Source**. The script moves to the
repository root by itself, so it can be started from any working directory. It takes about one
minute (the 500 repeated 70/30 splits take most of the time) and is reproducible (`set.seed(2026)`).

## What the script does

1. **Import** the training and testing column blocks of each genotype sheet.
2. **Clean and reshape** to one row per ewe per lambing. Blank cells (no recorded lambing at that
   parity) are excluded, not set to 0, because the data do not say what a blank means.
3. **Fit** `multiple ~ genotype + parity` on the training set (`glm`, logit link) and report
   coefficients, odds ratios, likelihood ratio tests, the genotype x parity interaction test, an
   additivity test (0/1/2 coding vs dummy coding), VIF and a mixed model with a random ewe intercept.
4. **Predict** the testing ewes with the fixed training coefficients and compute the confusion
   matrix, AUC, Brier score and log-loss against a baseline model.
5. **Robustness**: repeat the 70/30 split 500 times (split by ewe, stratified by genotype).
6. **Figures** (`figures/fig1`-`fig8`).

## Expected key results (training set, n = 922 lambings from 622 ewes)

| Term | Coefficient | Odds ratio |
|---|---|---|
| Intercept (++, parity 1) | -0.167 | 0.85 |
| Genotype B+ | 1.419 | 4.13 |
| Genotype BB | 1.834 | 6.26 |
| Parity 2 | 0.174 | 1.19 |
| Parity 3 | 0.735 | 2.09 |

Testing set (392 lambings from 268 ewes): AUC = 0.641; mean AUC over 500 random splits = 0.665.

## Data source

The data come from the supplementary tables of Miao, X. et al. Ovarian proteomic study reveals the
possible molecular mechanism for hyperprolificacy of Small Tail Han sheep. *Sci. Rep.* **6**, 27606
(2016), https://doi.org/10.1038/srep27606. The training/testing assignment of the ewes is given in
`DATA_EDITED2.xlsx`.

## Authors

[Author 1], [Author 2] - BioMaths 374, Stellenbosch University.
