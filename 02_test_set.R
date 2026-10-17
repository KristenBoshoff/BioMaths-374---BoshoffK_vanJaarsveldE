# 02_test_set.R
# Builds the TESTING set (30% of ewes) in the same way as the training set.
# Needs read_block() and clean_num() from 01_import_and_model.R.
library(dplyr)
library(tidyr)

raw_test <- rbind(
  read_block("++", 11:15, 3, "test"),
  read_block("B+", 19:23, 4, "test"),
  read_block("BB", 20:24, 3, "test"))
nrow(raw_test)   

test <- raw_test %>%
  pivot_longer(p1:p3, names_to = "parity", values_to = "litter") %>%
  mutate(litter = clean_num(litter)) %>%
  filter(!is.na(litter)) %>%
  mutate(parity   = factor(sub("p", "", parity), levels = c("1","2","3")),
         genotype = factor(genotype, levels = c("++","B+","BB")),
         multiple = as.integer(litter >= 2))
nrow(test)
