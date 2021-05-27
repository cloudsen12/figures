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
  BiocManager, reticulate, geojsonsf
)

# Load functions
source("utils.R")

# Initialize gee
ee_Initialize()

# Datasets
# 1. irish            (206 scene)
# 2. sparcs           (80  scene)
# 3. s2_hollstein     (59  scene)
# 4. baetens_hagolle  (38  scene)
# 5. cloud_catalog    (513 scene)
# 6. biome8           (96  scene)
# 7. 38cloud          (38  scene)
# 8. 95cloud          (95  scene)

# Cound labeling by dataset
table_bhlle <-
  countlabeling(
    dataset = "baetens_hagolle",
    path = "dataset/baetens_hagolle"
  )

# Save table ----
saveRDS(table_bhlle, file = "rds/table_bhlle.rds")