# /*
# MIT License
#
# Copyright (c) [2022] [CloudSEN12 team]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Load packages ----
library(tidyverse)
library(magick)
library(readr)
library(stars)

library(broom)
library(cvms)
library(ggimage)
library(rsvg)
library(ggnewscale)


# Donwload the comparison folder here:
# https://drive.google.com/file/d/1Sg4qC05Oic2emXv29uB6I2w7bVUa7MCd
LABEL_FOLDER <- "/media/csaybar/58059B472A3AA231/comparison/"

# list the files before the control quality
old_files <- list.files(
  path = sprintf("%s/old", LABEL_FOLDER),
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)


# get ROI and s2 ID
llt01_roi_id <- gsub("(.*)__(.*)__.*", "\\1", basename(old_files))
llt01_s2_id <- gsub("(.*)__(.*)__.*", "\\2", basename(old_files))

# list the files after control quality
new_folder <- sprintf("%s/new", LABEL_FOLDER)
new_files <- sprintf("%s/%s__%s.tif", new_folder, llt01_roi_id, llt01_s2_id)

# Generate a confusion matrix for each IP
confusion_metric_r <- list()
for(index in 1:10000) {
  print(index)
  old_gt <- read_stars(old_files[index])[[1]] %>% as.numeric()
  new_gt <- read_stars(new_files[index])[[1]] %>% as.numeric()
  old_gt <- c(old_gt, 0, 1, 2, 3)
  new_gt <- c(new_gt, 0, 1, 2, 3)
  mtx <- table(new_gt, old_gt)
  confusion_metric_r[[index]] <- mtx  - diag(1,nrow = 4, ncol = 4)
}

# Merge everything together
total_mx <- Reduce(`+`, confusion_metric_r)
total_mx2 <- total_mx[-3,-3]
sum(diag(total_mx))/sum(total_mx)
sum(diag(total_mx2))/sum(total_mx2)
# write_csv(as.data.frame(total_mx), "figure06/preload.csv")
# total_mx <- read_csv("figure08/preload.csv")$Freq

# Prepare data for plot_confusion_matrix
classes <- c("Clear", "Thick\nCloud", "Thin\nCloud", "Cloud\nShadow")
conf_mat <- tibble(
  Prediction = rep(classes, 4),
  Target = sapply(classes, function(x) rep(x, 4)) %>% as.character(),
  N = as.numeric(total_mx)
)
conf_mat$Prediction <- factor(conf_mat$Prediction, classes %>% rev())
conf_mat$Target <- factor(conf_mat$Target, classes %>% rev())

# ----------------------------
# Plot confusion matrix - I
# ----------------------------
plot_cm <-
  plot_confusion_matrix(
    conf_mat,
    add_sums = FALSE,
    add_row_percentages = TRUE,
    add_col_percentages = TRUE,
    add_counts = TRUE,
    add_normalized = TRUE,
    diag_percentages_only = TRUE,
    palette = "Blues",
    add_arrows = FALSE,
    font_normalized = font(size = 4),
    font_row_percentages = font(size = 3.2),
    font_col_percentages = font(size = 3.2),
    sums_settings = sum_tile_settings(
      palette = "Greens",
      label = "Total",
      tc_tile_border_color = "black",
      tc_tile_border_size = .5
    )
  ) +
  ggplot2::labs(
    x = "Labels without control quality",
    y = "Labels with control quality"
    ) +
  theme_bw() +
  coord_cartesian(expand = F) +
  theme(
    axis.text.x = element_text(
      size = 11, colour = "black",
      family = "Source Sans Pro",
      angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 11, color = "black",
      family = "Source Sans Pro",
      angle = 90,
      vjust = .6,
      hjust = .5
    ),
    axis.title.x = element_text(
      size = 12, color = "black",
      family = "Source Sans Pro", face = "bold"
    ),
    axis.title.y = element_text(
      size = 12, color = "black",
      family = "Source Sans Pro", face = "bold"
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = 1, color = "black")
  )
plot_cm

# Save ggplot object
ggsave(
  plot = plot_cm, "figure08/figure08_01.png",
  width = 13, height = 13, units = "cm", dpi = 500
)

# Trim figure ----
img <-
  magick::image_read("figure08/figure08_01.png", strip = TRUE) %>%
  magick::image_trim() %>%
  magick::image_border("white", "50x50")

# Save figure ----
magick::image_write(img, path = "figure08/figure08_01.png", format = "png")


# ----------------------------
# Plot confusion matrix - II
# ----------------------------
plot_cm <-
  plot_confusion_matrix(
    conf_mat,
    add_sums = FALSE,
    add_row_percentages = FALSE,
    add_col_percentages = FALSE,
    add_counts = TRUE,
    add_normalized = TRUE,
    diag_percentages_only = TRUE,
    palette = "Blues",
    add_arrows = FALSE,
    font_normalized = font(size = 4),
    font_row_percentages = font(size = 3.2),
    font_col_percentages = font(size = 3.2),
    sums_settings = sum_tile_settings(
      palette = "Greens",
      label = "Total",
      tc_tile_border_color = "black",
      tc_tile_border_size = .5
    )
  ) +
  ggplot2::labs(
    x = "Labels without control quality",
    y = "Labels with control quality"
  ) +
  theme_bw() +
  coord_cartesian(expand = F) +
  theme(
    axis.text.x = element_text(
      size = 11, colour = "black",
      family = "Source Sans Pro",
      angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 11, color = "black",
      family = "Source Sans Pro",
      angle = 90,
      vjust = .6,
      hjust = .5
    ),
    axis.title.x = element_text(
      size = 12, color = "black",
      family = "Source Sans Pro", face = "bold"
    ),
    axis.title.y = element_text(
      size = 12, color = "black",
      family = "Source Sans Pro", face = "bold"
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = 1, color = "black")
  )
plot_cm

# Save ggplot object
ggsave(
  plot = plot_cm, "figure08/figure08_02.png",
  width = 13, height = 13, units = "cm", dpi = 500
)

# Trim figure ----
img <-
  magick::image_read("figure08/figure08_02.png", strip = TRUE) %>%
  magick::image_trim() %>%
  magick::image_border("white", "50x50")

# Save figure ----
magick::image_write(img, path = "figure08/figure08_02.png", format = "png")
