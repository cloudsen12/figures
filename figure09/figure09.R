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
library(rworldmap)
library(tidyverse)
library(cowplot)
library(raster)
library(magick)
library(readr)
library(rgeos)
library(proj4)
library(tmap)
library(sf)
library(scales)

# Load dependency functions ----
source("src/utils.R")

# Define projection ----
crs_goode <- "+proj=igh"

# Load dataset ----
cloudsen12_db <- read_csv("data/cloudsen12_metadata.csv") %>%
  group_by(roi_id) %>%
  slice(1)
cloudsen12_sf <- st_sf(cloudsen12_db["test"], geometry = st_as_sfc(cloudsen12_db$proj_centroid))


# countries boundaries
world_sf <-
  st_as_sf(getMap(resolution = "low")) %>%
  st_transform(crs_goode)

# global boundaries
world_sf_base <-
  st_read(
    dsn = "data/hexagonGrid.gpkg",
    layer = "hexagonGrid"
  )

# Modify dataset to plot Figure 01 ----
st_crs(cloudsen12_sf) <- 4326
cloudsen12_sf <-
  cloudsen12_sf %>%
  mutate(row.id = 1:n()) %>%
  left_join(
    st_within(
      st_transform(cloudsen12_sf, crs_goode),
      world_sf_base
    ) %>%
      as.data.frame() %>%
      as_tibble()
  )

world_sf_base <-
  mutate(world_sf_base, col.id = 1:n()) %>%
  left_join(
    as.tibble(cloudsen12_sf) %>%
      dplyr::select(-geometry)
  ) %>%
  dplyr::filter(
    !is.na(test)
  )

# Goode homolosine Grid ----
hgoode_grid <- homolosine_goode_grid()
goode_without <- hgoode_grid$goode_without
goode_outline <- hgoode_grid$goode_outline
xlim <- hgoode_grid$xlim
ylim <- hgoode_grid$ylim

# Simple grid -------------------------------------------------------------
world_sf_base$test <- as.factor(world_sf_base$test)
hexaMap <-
  ggplot(world_sf_base) +
  geom_sf(
    data = st_graticule(
      lat = seq(-90, 90, by = 15),
      lon = seq(-180, 180, by = 15)
    ), color = "black", size = .05
  ) +
  geom_sf(
    data = world_sf, fill = "white",
    color = NA, size = 0.5 / .pt
  ) +
  geom_sf(mapping = aes(fill = test), color = NA) +
  geom_sf(data = world_sf, fill = NA, color = "black", size = 0.5 / .pt) +
  scale_fill_manual(
    values = c("#9d9d9d", "#373737"),
    labels = c("training\ndataset", "testing\ndataset")
  ) +
  geom_sf(data = goode_without, fill = "white", color = NA) +
  geom_sf(
    data = goode_outline, fill = NA,
    color = "black", size = 1.2 / .pt
  ) +
  coord_sf(
    crs = crs_goode, xlim = 0.95 * xlim,
    ylim = 0.95 * ylim, expand = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),
    minor_breaks = seq(-90, 90, 15)
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key = element_rect(colour = "black"),
    legend.title = element_blank(),
    legend.text =
      element_text(
        size = 9, family = "Source Sans Pro"
        # face = "bold"
      ),
    panel.background =
      element_rect(
        fill = "#cae7f8", color = "white", size = 1
      ),
    panel.grid.major = element_blank(),
  )


# Save simple grid ----
ggsave(
  plot = hexaMap, "figure09/figure09.png",
  width = 20, height = 15, units = "cm", dpi = 500
)

# Trim figure ----
img <-
  magick::image_read(
    "figure09/figure09.png",
    strip = TRUE
  ) %>%
  image_trim() %>%
  image_border("white", "50x50")

# Save figure ----
image_write(
  img,
  path = "figure09/figure09.png",
  format = "png", quality = 100
)
