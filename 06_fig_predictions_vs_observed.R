# 06_fig_predictions_vs_observed.R
# Figure: testing set, model predictions (lines) vs observed data (crosses). Needs 03.

library(dplyr); library(ggplot2)

grid <- expand.grid(genotype = c("++", "B+", "BB"), parity = 1:3)
grid$p <- p_multiple(grid$genotype, grid$parity)


obs_test <- test %>%
  group_by(genotype, parity) %>%
  summarise(p = mean(multiple), .groups = "drop")

plot_test <- ggplot(grid, aes(factor(parity), p, colour = genotype, group = genotype)) +
  geom_line() + geom_point(size = 3) +
  geom_point(data = obs_test, aes(factor(parity), p, colour = genotype),
             shape = 4, size = 4, stroke = 1.2) +
  labs(x = "Parity", y = "P(multiple birth)", colour = "Genotype",
       title = "Test set: Model predictions (lines) vs Observed data (crosses)")

print(plot_test)
ggsave("figures/fig_predictions_vs_observed.png", plot_test, width = 7, height = 4.5, dpi = 300, bg = "white")
