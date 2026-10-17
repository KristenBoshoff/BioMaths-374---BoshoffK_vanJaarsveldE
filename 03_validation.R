# 03_validation.R
# Defines the prediction function from the fitted coefficients and tests the model
# on the unseen testing ewes (predicted vs observed, AUC, Brier score, confusion matrix).
library(dplyr)
library(pROC)

b <- coef(fit)   

p_multiple <- function(genotype, parity) {
  lp <- b["(Intercept)"] +
    ifelse(genotype == "B+", b["genotypeB+"], 0) +
    ifelse(genotype == "BB", b["genotypeBB"], 0) +
    ifelse(parity == 2, b["parity2"], 0) +
    ifelse(parity == 3, b["parity3"], 0)
  unname(1 / (1 + exp(-lp)))
}

p_multiple("BB", 3)   
p_multiple("++", 1)   

test$phat <- p_multiple(test$genotype, test$parity)
test %>% group_by(genotype, parity) %>%
  summarise(n = n(), observed = mean(multiple), predicted = mean(phat), .groups = "drop")
library(pROC)
auc(roc(test$multiple, test$phat))        # expect about 0.64
mean((test$phat - test$multiple)^2)       # Brier score, about 0.159
table(Predicted = test$phat >= 0.7744, Observed = test$multiple)   # confusion matrix
