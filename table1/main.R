#' @author  The CloudSEN12 team

# Removing all objects including loaded libraries
rm(list = ls(all = TRUE))

# Installing and loading packages
if (!require("pacman")) {
  install.packages("pacman")
} else if (!require("rhdf5")) {
  library(BiocManager)
  BiocManager::install("rhdf5")
}

# Loading packages
pacman::p_load(
  tidyverse, rgee, filesstrings, magrittr, raster, janitor, sf, BiocManager
)


BiocManager::install("rhdf5")

# Save table ----
saveRDS(
  mutate_all(df, ~ replace(., is.na(.), 0)),
  file = "rds/irish.rds"
)