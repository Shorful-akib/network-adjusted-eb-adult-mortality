# Packages, paths, and constants shared across all scripts.

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(siblingsurvival)     # remotes::install_github("...siblingsurvival")
library(surveybootstrap)

DATA_DIR <- "data_raw"
OUT_DIR  <- "outputs"
dir.create(OUT_DIR, showWarnings = FALSE)

AGE_LABELS <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
AGE_LO     <- c(15, 20, 25, 30, 35, 40, 45)
X_CENTERED <- -3:3          # centered age score, used only in 09 (optional trend model)

N_BOOT <- 500   # matches manuscript Section 2.6: "A total of B = 500 replicate weight vectors"
