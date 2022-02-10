# /*
# MIT License
#
# Copyright (c) [2018] [CloudSEN12 team]
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
#
# */

library(rgee)
library(tmap)
library(sf)

# Load datasets -----------------------------------------------------------
cloudsen12_sf <- read_sf("data/cloudsen12_db.geojson")
data("World")
World <- st_transform(World, 32662)



# Create a world geoviz ---------------------------------------------------
tm_shape(World) +
  tm_polygons("white", lwd = .5) +
  tm_graticules(alpha=0.8, lwd = 0.5, labels.show = FALSE) +
  tm_shape(cloudsen12_sf) +
  tm_bubbles(size = 0.01, border.lwd = 0) +
  tmap_style("natural") +
  tm_layout(
    scale = .8,
    frame = FALSE,
    frame.lwd = NA,
    panel.label.bg.color = NA,
    attr.outside = TRUE
  ) -> sen2_tmap


# Save the map ------------------------------------------------------------
tmap_save(sen2_tmap, "figure_03.png", width = 1865, height = 1165)

