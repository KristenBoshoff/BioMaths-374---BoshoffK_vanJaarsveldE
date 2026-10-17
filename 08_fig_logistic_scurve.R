# 08_fig_logistic_scurve.R
# Figure: logistic S-curve with the 9 fitted groups. Needs 'b' and 'test' from 01-03.

library(ggplot2)
library(dplyr)

z_fun <- function(genotype, parity) {
  unname(b["(Intercept)"] +
           ifelse(genotype == "B+", b["genotypeB+"], 0) +
           ifelse(genotype == "BB", b["genotypeBB"], 0) +
           ifelse(parity == 2, b["parity2"], 0) +
           ifelse(parity == 3, b["parity3"], 0))
}


curve <- data.frame(z = seq(-4, 4, length.out = 300))
curve$p <- 1 / (1 + exp(-curve$z))


groups <- expand.grid(genotype = c("++","B+","BB"), parity = c("1","2","3"))
groups$z <- z_fun(groups$genotype, as.numeric(as.character(groups$parity)))
groups$p <- 1 / (1 + exp(-groups$z))

obs <- test %>% group_by(genotype, parity) %>%
  summarise(observed = mean(multiple), n = n(), .groups = "drop")
groups <- merge(groups, obs, by = c("genotype", "parity"))

ggplot() +
  geom_line(data = curve, aes(z, p), linewidth = 1, colour = "grey30") +
  geom_point(data = groups, aes(z, p, colour = genotype, shape = parity), size = 4) +
  geom_point(data = groups, aes(z, observed, colour = genotype, shape = parity),
             size = 3, alpha = 0.5, stroke = 1.2) +
  labs(x = "Linear predictor z (log-odds)", y = "P(multiple birth)",
       colour = "Genotype", shape = "Parity",
       title = "Logistic S-curve: fitted groups on the curve")

ggsave("figures/fig_logistic_scurve.png", width = 7, height = 4.5, dpi = 300, bg = "white")
