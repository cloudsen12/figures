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

library(tidyverse)
library(magick)
library(Hmisc)

# Build data before plot ----
other.dset <-
  readxl::read_xls("data/dataset.xls") %>%
  dplyr::filter(Dataset %nin% "cloudSEN12") %>%
  arrange(pixels) %>%
  mutate(
    pixels = round(pixels, 3),
    lab = as.character(pixels),
    y = pixels + .8
  )

other.dset$y[6:7] <- (other.dset$pixels)[6:7] / 2

dset <-
  other.dset %>%
  bind_rows(
    readxl::read_xls("data/dataset.xls") %>%
      dplyr::filter(Dataset %in% "cloudSEN12") %>%
      mutate(id = c(3, 1, 2)) %>%
      arrange(desc(id)) %>%
      mutate(
        pixels = round(pixels, 3),
        lab = sprintf("%1s", round(pixels, 1)),
        ycum = cumsum(pixels),
        y = ycum - (pixels / 2)
      ) %>%
      arrange(id)
  ) %>%
  arrange(desc(row_number())) %>%
  mutate(Dataset = as_factor(Dataset))

# Plot ----
barplot <-
  ggplot(
    dset,
    aes(
      x = Dataset, y = pixels, linetype = type,
      fill = type, group = sensor
    )
  ) +
  labs(x = NULL, y = "\n# of pixels [bill.]") +
  geom_bar(
    position = "stack", stat = "identity",
    colour = "black"
  ) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(
    breaks = seq(0, 4.6, 1.5),
    limits = c(0, 5.35)
  ) +
  theme_bw() +
  theme(
    legend.margin = margin(3, 7, 7, 7),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(.5, "cm"),
    legend.title = element_blank(),
    legend.position = c(.67, .88),
    legend.direction = "vertical",
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
      angle = 0, vjust = .6
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
  geom_text(
    aes(x = Dataset, y = y, label = lab),
    size = 4,
    angle = 0,
    color = "black",
    family = "Source Sans Pro",
    fontface = "bold"
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_flip()

# Save ggplot object ----
ggsave(
  plot = barplot, "figure02/figure02.png",
  width = 13, height = 13, units = "cm", dpi = 500
)

# Trim figure ----
img <-
  magick::image_read("figure02/figure02.png", strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

# Save figure ----
image_write(img, path = "figure02/figure02.png", format = "png")
