library(rgee)
library(tmap)
library(sf)

data("World")

# Load datasets -----------------------------------------------------------
cloudsen12_sf <- read_sf("data/cloudsen12_db.geojson")


tm_shape(World) +
  tm_polygons("white", lwd = .5) +
  tm_graticules(alpha=0.8, lwd = 0.5, labels.size = 0.65) +
  tm_shape(cloudsen12_sf) +
  tm_bubbles(size = 0.025) +
  tmap_style("natural") +
  tm_layout(
    scale = .8,
    frame = FALSE,
    frame.lwd = NA,
    panel.label.bg.color = NA,
    attr.outside = TRUE
  ) -> sen2_tmap

tmap_save(sen2_tmap, "img1/figure_01.svg", width = 1865, height = 1165)
tmap_save(sen2_tmap, "img1/figure_01.pdf", width = 1865, height = 1165)
