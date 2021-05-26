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
  rgdal, tidyverse, rgee, filesstrings, magrittr, raster, janitor, sf,
  BiocManager, reticulate
)

# Load functions
source("utils.R")

# Datasets
# 1. irish
# 2. sparcs
# 3. s2_hollstein
# 4. baetens_hagolle
# 5. cloud_catalog
# 6. biome8
# 7. 38cloud
# 8. 95cloud

# Cound labeling by dataset
table_biome8 <- countlabeling(dataset = "biome8", path = "dataset/biome8")

# Save table ----
saveRDS(table_biome8, file = "rds/irish.rds")