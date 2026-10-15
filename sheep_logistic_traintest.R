# ==============================================================================
# Logistic regression: probability of a multiple birth in sheep (BioMaths 374)
#
# Question : how do FecB genotype (++, B+, BB) and parity (1, 2, 3) change the
#            probability that a lambing is a multiple birth (litter size >= 2)?
# Design   : 70% of ewes = TRAINING set (the model is fitted here only)
#            30% of ewes = TESTING set  (used only to check the fitted model)
# Data     : data/DATA_EDITED2.xlsx  (sheets "++", "B+", "BB"; the train and
#            test ewes sit in separate column blocks of each sheet)
#
# HOW TO RUN (from the repository root, i.e. the folder containing README.md):
#     Rscript R/sheep_logistic_traintest.R
#   or in RStudio: open the file and click "Source" (the script finds the root).
# Runtime: about 1 minute (most of it is the 500 repeated splits in section 7b).
# Outputs : figures/*.png and output/*.csv (created automatically)
# ==============================================================================

## 0. SETUP --------------------------------------------------------------------
# Install any missing packages, then load them.
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "car", "lme4", "emmeans")
missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs)) install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# Move to the repository root so that the relative paths below work.
cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
if (length(script_path) == 1) {
  setwd(file.path(dirname(normalizePath(script_path)), ".."))        # run with Rscript
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), ".."))  # run in RStudio
}
file <- "data/DATA_EDITED2.xlsx"
if (!file.exists(file)) stop("Cannot find ", file, ". Run the script from the repository root.")
dir.create("figures", showWarnings = FALSE); dir.create("output", showWarnings = FALSE)
set.seed(2026)   # makes the 500 random splits reproducible

## 1. IMPORT: read the train and test blocks from each genotype sheet ----------
# Column positions (A=1): ++ test K:O, train U:Y | B+ train J:N, test S:W | BB train K:O, test T:X
read_block <- function(sheet, cols, first_row, set) {
  x <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  b <- x[first_row:nrow(x), cols]
  names(b) <- c("ID", "genotype", "p1", "p2", "p3")
  b %>% filter(!is.na(ID)) %>% mutate(set = set)
}
raw <- bind_rows(
  read_block("++", 21:25, 3, "train"), read_block("++", 11:15, 3, "test"),
  read_block("B+", 10:14, 4, "train"), read_block("B+", 19:23, 4, "test"),
  read_block("BB", 11:15, 3, "train"), read_block("BB", 20:24, 3, "test"))

## 2. CLEAN and reshape to one row per ewe per lambing -----------------------
clean_num <- function(x) suppressWarnings(as.numeric(trimws(x)))   # blanks / spaces -> NA
raw <- raw %>% mutate(ID = trimws(ID), genotype = trimws(genotype), across(p1:p3, clean_num))
stopifnot(!any(duplicated(raw$ID)))                                 # no ewe in both sets

long <- raw %>%
  pivot_longer(p1:p3, names_to = "parity", values_to = "litter") %>%
  filter(!is.na(litter)) %>%
  mutate(parity   = factor(sub("p", "", parity)),                       # 1, 2, 3 (reference = 1)
         genotype = factor(genotype, levels = c("++", "B+", "BB")),     # reference = ++
         multiple = as.integer(litter >= 2))                            # 1 = twins or more

train <- filter(long, set == "train"); test <- filter(long, set == "test")

# Blank cells = no recorded lambing at that parity. They are EXCLUDED (not set to 0),
# because the data do not say whether a blank means "did not lamb" or "no record".
cat("Possible ewe x parity cells in the training set:", 3 * n_distinct(raw$ID[raw$set == "train"]),
    "| with a litter size:", nrow(train), "| blank (excluded):", 3 * n_distinct(raw$ID[raw$set == "train"]) - nrow(train), "\n")
cat("Ewes in file:", n_distinct(raw$ID), " | train ewes:", n_distinct(raw$ID[raw$set=="train"]),
    " test ewes:", n_distinct(raw$ID[raw$set=="test"]), "\n")
cat("Lambing records - train:", nrow(train), " test:", nrow(test), "\n")
print(table(train$genotype, train$parity)); print(table(test$genotype, test$parity))

## 3. FIT THE MODEL ON THE TRAINING DATA ONLY --------------------------------
fit <- glm(multiple ~ genotype + parity, data = train, family = binomial(link = "logit"))
print(summary(fit))
ci <- suppressMessages(confint(fit))
sm <- summary(fit)$coefficients
coef_tab <- data.frame(term = rownames(sm), beta = sm[,1], SE = sm[,2], z = sm[,3], p = sm[,4],
                       OR = exp(sm[,1]), OR_low = exp(ci[,1]), OR_high = exp(ci[,2]))
print(round(coef_tab[,-1], 4)); write.csv(coef_tab, "output/coefficients_train.csv", row.names = FALSE)

null <- glm(multiple ~ 1, data = train, family = binomial)
cat("\nOverall LRT vs null:\n"); print(anova(null, fit, test = "Chisq"))
cat("\nPer-predictor LRT:\n"); print(Anova(fit, test = "LR"))
fit_int <- glm(multiple ~ genotype * parity, data = train, family = binomial)
cat("\nInteraction test:\n"); print(anova(fit, fit_int, test = "Chisq"))
# Additivity check: is the BB effect exactly twice the B+ effect? (0/1/2 coding vs dummy coding)
train$copies <- as.numeric(train$genotype) - 1            # 0, 1, 2 copies of the FecB mutation
fit_add <- glm(multiple ~ copies + parity, data = train, family = binomial)
cat("\nAdditivity test (0/1/2 coding vs dummy coding):\n"); print(anova(fit_add, fit, test = "Chisq"))
cat("AIC additive:", round(AIC(fit_add), 1), " AIC dummy:", round(AIC(fit), 1), "\n")
cat("\nPairwise genotype contrasts:\n")
print(summary(pairs(emmeans(fit, ~ genotype), type = "response"), infer = TRUE))
cat("\nVIF:\n"); print(vif(fit))
fit_mm <- glmer(multiple ~ genotype + parity + (1 | ID), data = train, family = binomial)
cat("\nMixed model (random intercept per ewe), TRAIN:\n"); print(summary(fit_mm)$coefficients)

## 4. HELPERS ------------------------------------------------------------------
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1*(n1+1)/2)/(n1*n0) }
logloss <- function(y, p) -mean(y*log(p) + (1-y)*log(1-p))

## 5. TRAINING PERFORMANCE ------------------------------------------------------
train$phat <- fitted(fit)
cutoff <- mean(train$multiple)                       # classify as "multiple" if p > training prevalence
cat("\nTRAIN: prevalence (cut-off) =", round(cutoff, 4), " AUC =", round(auc(train$multiple, train$phat), 4),
    " McFadden R2 =", round(1 - fit$deviance/fit$null.deviance, 4), " AIC =", round(AIC(fit), 1), "\n")

## 6. TEST PERFORMANCE (data the model has never seen) -------------------------------
test$phat <- predict(fit, newdata = test, type = "response")
test$pred <- as.integer(test$phat > cutoff)
cm <- table(Predicted = test$pred, Actual = test$multiple); print(cm)
TP <- cm["1","1"]; TN <- cm["0","0"]; FP <- cm["1","0"]; FN <- cm["0","1"]
metrics <- data.frame(
  n_test = nrow(test), prevalence_test = mean(test$multiple),
  accuracy = (TP+TN)/sum(cm), sensitivity = TP/(TP+FN), specificity = TN/(TN+FP),
  AUC = auc(test$multiple, test$phat),
  Brier_model = mean((test$phat - test$multiple)^2),
  Brier_baseline = mean((cutoff - test$multiple)^2),      # predicting the training prevalence for everyone
  logloss_model = logloss(test$multiple, test$phat),
  logloss_baseline = logloss(test$multiple, rep(cutoff, nrow(test))))
print(round(t(metrics), 4)); write.csv(metrics, "output/test_metrics.csv", row.names = FALSE)
cat("Test accuracy of always predicting 'multiple':", mean(test$multiple), "\n")

## 7. PREDICTED (from training fit) vs OBSERVED in the TEST set --------------------
grid <- expand.grid(genotype = levels(long$genotype), parity = levels(long$parity))
pr <- predict(fit, newdata = grid, type = "link", se.fit = TRUE)
grid$prob <- plogis(pr$fit); grid$lo <- plogis(pr$fit - 1.96*pr$se.fit); grid$hi <- plogis(pr$fit + 1.96*pr$se.fit)
summ <- function(d, tag) d %>% group_by(genotype, parity) %>%
  summarise(n = n(), k = sum(multiple), obs = mean(multiple), .groups = "drop") %>%
  mutate(obs_lo = mapply(function(k, n) binom.test(k, n)$conf.int[1], k, n),
         obs_hi = mapply(function(k, n) binom.test(k, n)$conf.int[2], k, n), set = tag)
obs_train <- summ(train, "train"); obs_test <- summ(test, "test")
res <- grid %>% left_join(select(obs_train, genotype, parity, n_train = n, obs_train = obs), by = c("genotype","parity")) %>%
  left_join(select(obs_test, genotype, parity, n_test = n, k_test = k, obs_test = obs, test_lo = obs_lo, test_hi = obs_hi), by = c("genotype","parity")) %>%
  mutate(test_inside_CI = obs_test >= lo & obs_test <= hi)
print(res); write.csv(res, "output/predicted_vs_observed.csv", row.names = FALSE)

# by genotype only (test set)
print(test %>% group_by(genotype) %>% summarise(n = n(), observed = mean(multiple), predicted = mean(phat)))

# worked example
b <- coef(fit); lp <- b["(Intercept)"] + b["genotypeBB"] + b["parity3"]
cat("\nBB, parity 3: linear predictor =", lp, " P(multiple) =", 1/(1+exp(-lp)), "\n")

## 7b. ROBUSTNESS: is the test result just luck of this one split? --------------------
# Repeat the 70/30 split 500 times (stratified by genotype, split by EWE), refit, test.
ewes <- raw %>% distinct(ID, genotype)
one_split <- function() {
  tr_ids <- ewes %>% group_by(genotype) %>% slice_sample(prop = 0.7) %>% pull(ID)
  tr <- filter(long, ID %in% tr_ids); te <- filter(long, !ID %in% tr_ids)
  m <- glm(multiple ~ genotype + parity, data = tr, family = binomial)
  p <- predict(m, newdata = te, type = "response"); base <- mean(tr$multiple)
  c(AUC = auc(te$multiple, p), Brier_gain = mean((base - te$multiple)^2) - mean((p - te$multiple)^2),
    logloss_gain = logloss(te$multiple, rep(base, nrow(te))) - logloss(te$multiple, p))
}
rep_res <- as.data.frame(t(replicate(500, one_split())))
rob <- data.frame(measure = names(rep_res), mean = sapply(rep_res, mean), lo = sapply(rep_res, quantile, 0.025),
                  hi = sapply(rep_res, quantile, 0.975), pct_positive = sapply(rep_res, function(v) mean(v > 0)))
print(rob); write.csv(rob, "output/robustness_500_splits.csv", row.names = FALSE)
write.csv(rep_res, "output/robustness_500_splits_raw.csv", row.names = FALSE)

## 8. FIGURES ------------------------------------------------------------------------
cols <- c("++" = "#8c6d4f", "B+" = "#5b8c3a", "BB" = "#2f6f8f")
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank()))
sv <- function(name, p, w = 7, h = 4.5) ggsave(paste0("figures/", name), p, width = w, height = h, dpi = 300, bg = "white")

f1 <- train %>% mutate(litter = factor(pmin(litter, 4), labels = c("1","2","3","4+"))) %>%
  count(genotype, litter) %>% group_by(genotype) %>% mutate(pct = n/sum(n))
sv("fig1_litter_distribution.png", ggplot(f1, aes(litter, pct, fill = genotype)) + geom_col(position = "dodge") +
  scale_fill_manual(values = cols) + scale_y_continuous(labels = scales::percent) +
  labs(x = "Litter size", y = "Share of lambings", fill = "FecB genotype", title = "Litter size by genotype (training set)"))

sv("fig2_observed_train.png", ggplot(obs_train, aes(parity, obs, colour = genotype, group = genotype)) +
  geom_pointrange(aes(ymin = obs_lo, ymax = obs_hi), position = position_dodge(0.4)) +
  scale_colour_manual(values = cols) + scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "Parity", y = "Observed proportion of multiple births", colour = "FecB genotype",
       title = "Observed multiple births, training set (95% CI)"))

sv("fig3_predicted_probability.png", ggplot(grid, aes(parity, prob, colour = genotype, group = genotype)) +
  geom_line(position = position_dodge(0.3)) + geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(0.3)) +
  scale_colour_manual(values = cols) + scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "Parity", y = "Predicted P(multiple birth)", colour = "FecB genotype",
       title = "Predicted probability (model fitted on training set)"))

fp <- coef_tab[-1, ]; fp$label <- c("B+ vs ++", "BB vs ++", "Parity 2 vs 1", "Parity 3 vs 1")
sv("fig4_odds_ratios.png", ggplot(fp, aes(OR, reorder(label, OR))) + geom_vline(xintercept = 1, linetype = 2) +
  geom_pointrange(aes(xmin = OR_low, xmax = OR_high), colour = "#2f6f8f") + scale_x_log10() +
  labs(x = "Odds ratio (log scale, 95% CI)", y = NULL, title = "Odds ratios (training set)"), h = 3.8)

xs <- seq(-3, 4, length.out = 300)
curve_df <- data.frame(x = xs, y = plogis(xs))
pts_df   <- data.frame(grid, x = qlogis(grid$prob), y = grid$prob)
sv("fig5_logistic_curve.png", ggplot() + geom_line(data = curve_df, aes(x = x, y = y), colour = "grey30", linewidth = 1) +
  geom_point(data = pts_df, aes(x = x, y = y, colour = genotype, shape = parity), size = 3) +
  scale_colour_manual(values = cols) + scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Linear predictor (b0 + genotype + parity effects)", y = "P(multiple birth)", colour = "Genotype", shape = "Parity",
       title = "The logistic curve and the fitted groups"))

# KEY VALIDATION FIGURE: model prediction (from train) vs observed test proportions
pd <- position_dodge(0.5)
sv("fig6_test_validation.png", ggplot() +
  geom_crossbar(data = grid, aes(parity, prob, ymin = lo, ymax = hi, colour = genotype, group = genotype),
                position = pd, width = 0.4, fill = NA, linewidth = 0.4, fatten = 3) +
  geom_point(data = res, aes(parity, obs_test, fill = genotype, group = genotype), shape = 21, size = 3.2, colour = "black", position = pd) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "Parity", y = "Probability of multiple birth", colour = "Genotype (model, 95% CI)", fill = "Genotype (test data)",
       title = "Test set: observed (dots) vs model prediction (bars)"), w = 8)

o <- order(test$phat, decreasing = TRUE); y <- test$multiple[o]
roc <- data.frame(fpr = c(0, cumsum(y == 0)/sum(y == 0)), tpr = c(0, cumsum(y == 1)/sum(y == 1)))
sv("fig7_roc_test.png", ggplot(roc, aes(fpr, tpr)) + geom_line(colour = "#2f6f8f", linewidth = 1) + geom_abline(linetype = 2) +
  coord_equal() + labs(x = "False positive rate", y = "True positive rate",
                       title = paste0("ROC curve, test set (AUC = ", round(auc(test$multiple, test$phat), 3), ")")), w = 5, h = 5)
sv("fig8_robustness_auc.png", ggplot(rep_res, aes(AUC)) + geom_histogram(bins = 30, fill = "#5b8c3a", colour = "white") +
  geom_vline(xintercept = metrics$AUC, colour = "#8c6d4f", linewidth = 1, linetype = 2) +
  geom_vline(xintercept = 0.5, colour = "grey40") +
  labs(x = "Test-set AUC", y = "Number of random splits", title = "Test AUC over 500 random 70/30 splits",
       subtitle = "Dashed = your split; grey = no discrimination (0.5)"))
cat("\nDone.\n")
