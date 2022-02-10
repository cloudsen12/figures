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
library(viridis)
library(readr)


# Load dataset ----
dataset <-
  read_csv("data/dataset.csv") %>%
  mutate(Overall = 100 - Overall)

# Build barplot
gmp <-
  ggplot(dataset, aes(fill = Class, y = Overall, x = Difficulty)) +
  labs(x = "Difficulty", y = "\n1 - Accuracy") +
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  scale_fill_manual(values = c("#ffffff", "#ff0000", "#ffff00", "#00ff00")) +
  scale_y_continuous(
    breaks = seq(0, 20, 5),
    limits = c(0, 20)
  ) +
  theme_bw() +
  theme(
    legend.margin = margin(3, 7, 7, 7),
    legend.key.width = unit(1.2, "cm"),
    legend.direction = "horizontal",
    legend.key.height = unit(.5, "cm"),
    legend.title = element_blank(),
    legend.position = c(.7, .15),
    legend.box = "horizontal",
    legend.text = element_text(size = 12, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 10, colour = "black",
      family = "Source Sans Pro",
      angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 12, color = "black",
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
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_flip()

# Save ggplot object
ggsave(
  plot = gmp, "figure06/figure06.png",
  width = 10, height = 13, units = "cm", dpi = 500
)

# Trim figure ----
img <-
  magick::image_read("figure06/figure06.png", strip = TRUE) %>%
  magick::image_trim() %>%
  magick::image_border("white", "50x50")

# Save figure ----
magick::image_write(img, path = "figure06/figure06.png", format = "png")
