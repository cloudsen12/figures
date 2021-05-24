#' @author  The CloudSEN12 team

# Removing all objects including loaded libraries
rm(list = ls(all = TRUE))

# Installing and loading packages
if (!require("pacman")) {
  install.packages("pacman")
}

# Loading packages
pacman::p_load(
  tidyverse, rgee, filesstrings, magrittr, raster, janitor, sf
)

# Save table ----
saveRDS(
  mutate_all(df, ~ replace(., is.na(.), 0)),
  file = "rds/irish.rds"
)