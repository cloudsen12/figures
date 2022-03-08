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

library(broom)
library(cvms)
library(ggimage)
library(rsvg)
library(ggnewscale)


# Simulate dataset
class <- c("Clear", "Cloud\nShadow", "Thick\nCloud", "Thin\nCloud")
set.seed(2022)
d_multi <-
  tibble(
    target = sample(class, 509, replace = T),
    prediction = sample(class, 509, replace = T)
  )

conf_mat <- cvms::confusion_matrix(
  targets = d_multi$target,
  predictions = d_multi$prediction
)

# Plot confusion matrix
plot <-
  plot_confusion_matrix(
    conf_mat$`Confusion Matrix`[[1]],
    add_sums = TRUE,
    add_row_percentages = TRUE,
    add_col_percentages = TRUE,
    add_counts = FALSE,
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
    x = "Class from manual clasification",
    y = "Classification result"
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

# Save ggplot object
ggsave(
  plot = plot, "figure06/figure06.png",
  width = 10, height = 10, units = "cm", dpi = 500
)

# Trim figure ----
img <-
  magick::image_read("figure06/figure06.png", strip = TRUE) %>%
  magick::image_trim() %>%
  magick::image_border("white", "50x50")

# Save figure ----
magick::image_write(img, path = "figure06/figure06.png", format = "png")
