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
dset <- tibble(
  Dataset = c(
    "S2-cloudSEN12", "S2-cloudSEN12", "S2-cloudSEN12", "L8-Biome8",
    "L8-95Cloud", "L8-38Cloud", "S2-cloudCatalog", "S2-CESBIO",
    "L8-SPARCS", "S2-Hollstein", "S2-WHUS2-CD", "S2-KappaZeta"
  ),
  pixels = c(2.591, 0.583, 1.523, 3.964, 3.737, 1.494, 0.535, 0.109, 0.080, 0.003, 4.273, 1.064),
  type = c("high-quality", "scribble", "no-annotation", rep(NA, 9)),
  sensor = c(
    "Sentinel-2/Sentinel-1", "Sentinel-2/Sentinel-1", "Sentinel-2/Sentinel-1",
    "Landsat8", "Landsat8", "Landsat8", "Sentinel-2", "Sentinel-2", "Landsat8",
    "Sentinel-2", "Sentinel-2", "Sentinel-2"
  ),
  lab = c("2.59", "0.58", "1.52", "3.96", "3.74", "1.49", "0.54", "0.11", "0.08", "0.003", "4.27", "1.06"),
  y = c(1.305, 2.890, 3.960, 1.980, 1.870, 0.770, 0.275, 0.258, 0.230, 0.190, 2.375, 0.545)
)

dset$Dataset <- factor(
  x = dset$Dataset,
  levels = c(
    "S2-cloudSEN12", "S2-WHUS2-CD", "L8-Biome8", "L8-95Cloud", "L8-38Cloud",
    "S2-KappaZeta", "S2-cloudCatalog", "S2-CESBIO", "L8-SPARCS", "S2-Hollstein"
  )
)

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
  scale_fill_manual(
    values = c("#999999", "#E69F00", "#56B4E9"),
    na.value = "#05ed79"
  ) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  scale_y_continuous(
    breaks = seq(0, 4.5, 1.5),
    limits = c(0, 4.8),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    legend.margin = margin(3, 7, 7, 7),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(.5, "cm"),
    legend.title = element_blank(),
    legend.position = c(.83, .87),
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
barplot

# Save ggplot object ----
ggsave(
  plot = barplot, "figure02/figure02.svg",
  width = 18, height = 12, units = "cm", dpi = 500
)

raster::extract()
# Trim figure ----
img <-
  magick::image_read("figure02/figure02.png", strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

# Save figure ----
image_write(img, path = "figure02/figure02.png", format = "png")
