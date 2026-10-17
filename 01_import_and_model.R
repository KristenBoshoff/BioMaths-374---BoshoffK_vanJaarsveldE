# 01_import_and_model.R
# Imports the TRAINING ewes, reshapes to one row per lambing,
# defines the outcome (multiple = 1 if litter size >= 2) and fits the logistic regression.
# Run all scripts from the project folder (the one containing R/ and data/).
library(readxl)
library(dplyr)
library(tidyr)
dir.create("figures", showWarnings = FALSE)

file <- "data/DATA_EDITED2.xlsx"   # path relative to the project folder
stopifnot(file.exists(file))

read_block <- function(sheet, cols, first_row, set) {
  x <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  b <- x[first_row:nrow(x), cols]
  names(b) <- c("ID", "genotype", "p1", "p2", "p3")
  b %>% filter(!is.na(ID)) %>% mutate(set = set)
}
raw <- rbind(
  read_block("++", 21:25, 3, "train"),
  read_block("B+", 10:14, 4, "train"),
  read_block("BB", 11:15, 3, "train"))
nrow(raw)        # expect 622 ewes (99 + 289 + 234)
clean_num <- function(x) suppressWarnings(as.numeric(trimws(x)))

train <- raw %>%
  pivot_longer(p1:p3, names_to = "parity", values_to = "litter") %>%
  mutate(litter = clean_num(litter)) %>%
  filter(!is.na(litter)) %>%
  mutate(parity   = factor(sub("p", "", parity)),
         genotype = factor(genotype, levels = c("++", "B+", "BB")),
         multiple = as.integer(litter >= 2))

nrow(train)                           
table(train$genotype, train$multiple)  

fit <- glm(multiple ~ genotype + parity, data = train,
           family = binomial(link = "logit"))
coef(fit)
