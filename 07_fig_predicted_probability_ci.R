# 07_fig_predicted_probability_ci.R
# Figure: predicted probability of a multiple birth with 95% CI. Needs 'fit' from 01.

library(dplyr); library(ggplot2)

grid <- expand.grid(genotype = factor(c("++","B+","BB"), levels = c("++","B+","BB")),
                    parity   = factor(1:3))
pr <- predict(fit, newdata = grid, type = "link", se.fit = TRUE)
grid$prob <- plogis(pr$fit)
grid$lo   <- plogis(pr$fit - 1.96 * pr$se.fit)
grid$hi   <- plogis(pr$fit + 1.96 * pr$se.fit)

ggplot(grid, aes(parity, prob, colour = genotype, group = genotype)) +
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(0.3)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "Parity", y = "Predicted P(multiple birth)", colour = "FecB genotype",
       title = "Predicted probability of a multiple birth (95% CI)")

ggsave("figures/fig_predicted_probability_ci.png", width = 7, height = 4.5, dpi = 300, bg = "white")
