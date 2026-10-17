# 04_fig_genotype_litter.R
# Figure: litter size by genotype (training set). Needs 'train' from 01.

library(dplyr)
library(ggplot2)
library(scales)   

cols <- c("++" = "#8c6d4f", "B+" = "#5b8c3a", "BB" = "#2f6f8f")


f1 <- train %>%
  mutate(litter = factor(pmin(litter, 4), labels = c("1", "2", "3", "4+"))) %>%
  count(genotype, litter) %>%
  group_by(genotype) %>%
  mutate(pct = n / sum(n))      


ggplot(f1, aes(litter, pct, fill = genotype)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = percent) +
  labs(x = "Litter size", y = "Share of lambings",
       fill = "FecB genotype",
       title = "Litter size by genotype (training set)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("figures/fig_litter_by_genotype.png", width = 7, height = 4.5, dpi = 300, bg = "white")
