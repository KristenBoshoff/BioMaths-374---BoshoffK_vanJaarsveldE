# 05_fig_parity_litter.R
# Figure: litter size by parity (training set). Needs 'train' from 01.

library(dplyr)
library(ggplot2)
library(scales)


cols_par <- c("1" = "#d9a441", "2" = "#c0603a", "3" = "#6a3d7c")

f2 <- train %>%
  mutate(litter = factor(pmin(litter, 4), labels = c("1", "2", "3", "4+")),
         parity = factor(parity)) %>%
  count(parity, litter) %>%
  group_by(parity) %>%
  mutate(pct = n / sum(n))          

ggplot(f2, aes(litter, pct, fill = parity)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_par) +
  scale_y_continuous(labels = percent) +
  labs(x = "Litter size", y = "Share of lambings",
       fill = "Parity",
       title = "Litter size by parity (training set)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("figures/fig_litter_by_parity.png", width = 7, height = 4.5, dpi = 300, bg = "white")
