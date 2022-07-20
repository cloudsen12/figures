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

# 1. Load packages --------------------------------------------------------
library(tidyverse)
library(ggridges)
library(ggplot2)
library(pals)
source("figure11/utils.R")

# 2. Create color pal -----------------------------------------------------
nicecolorbar <- c(
    "#fffffe", "#fffffe",
    rev(kovesi.linear_bgyw_15_100_c68(98))
)

# 3. Load BOA, Recall & Precision results ---------------------------------
metricsdb <- read_csv("figure11/cloudsen12_metrics.csv")
metricsdb <- metricsdb[metricsdb$CCOVER >= 0.05,]


metricsdb_precision <- metricsdb %>% dplyr::select(matches('PRECISION')) %>% dplyr::select(matches('exp01'))
metricsdb_recall <- metricsdb %>% dplyr::select(matches('RECALL')) %>% dplyr::select(matches('exp01'))
metricsdb_boa <- metricsdb %>% dplyr::select(matches('BOA')) %>% dplyr::select(matches('exp01'))

cloud_model_names <- c(
  "Human-level", "Sen2Cor", "KappaMask L1C", "KappaMask L2A",
  "Fmask", "U-Net MobileNetV2", "s2cloudless", "QA60",
  "CD-FCNN RGBI", "CD-FCNN RGBISWIR"
)

colnames(metricsdb_recall) <- cloud_model_names
colnames(metricsdb_precision) <- cloud_model_names
colnames(metricsdb_boa) <- cloud_model_names


metricsdb_recall_db <- metricsdb_recall %>%
  pivot_longer(everything(), names_to = c("model"), values_to = "error")

metricsdb_precision_db <- metricsdb_precision %>%
  pivot_longer(everything(), names_to = c("model"), values_to = "error")

metricsdb_boa_db <- metricsdb_boa %>%
  pivot_longer(everything(), names_to = c("model"), values_to = "error")

# 4. Plot density errors --------------------------------------------------
m1 <- generate_density_gradient(metricsdb_recall_db, "PA %", gradient_bar = nicecolorbar)
m2 <- generate_density_gradient(metricsdb_precision_db, "UA %", gradient_bar = nicecolorbar)
m3 <- generate_density_gradient(db = metricsdb_boa_db, xtext = "BOA %", gradient_bar = nicecolorbar)

# prepare plot to draw.io
m3 <- m3 +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  xlab("")

# 5. Save ggplot object ---------------------------------------------------
# ggsave(
#   plot = m1, "figure11/figure11_PA_densities.png",
#   width = 20, height = 10, units = "cm", dpi = 500
# )
# ggsave(
#   plot = m2, "figure11/figure11_UA_densities.png",
#   width = 20, height = 10, units = "cm", dpi = 500
# )
ggsave(
  plot = m3, "figure11/figure11_BOA_densities.png",
  width = 20, height = 10, units = "cm", dpi = 500
)


# 6. Trim figures ---------------------------------------------------------
# img01 <-
#   magick::image_read("figure11/figure11_PA_densities.png", strip = TRUE) %>%
#   magick::image_trim() %>%
#   magick::image_border("white", "10x10")
#
# img02 <-
#   magick::image_read("figure11/figure11_UA_densities.png", strip = TRUE) %>%
#   magick::image_trim() %>%
#   magick::image_border("white", "10x10")
img03 <-
  magick::image_read("figure11/figure11_BOA_densities.png", strip = TRUE) %>%
  magick::image_trim() %>%
  magick::image_border("white", "10x10")



# 7. Save figure ----------------------------------------------------------
# magick::image_write(img01, path = "figure11/figure11_PA_densities.png", format = "png")
# magick::image_write(img02, path = "figure11/figure11_UA_densities.png", format = "png")
magick::image_write(img03, path = "figure11/figure11_BOA_densities.png", format = "png")


# 8. relative probabilities as colobars  ----------------------------------
generate_png(db = metricsdb_precision, gradient_bar = nicecolorbar, output = "figure11/UA_colobar/")
generate_png(db = metricsdb_recall, gradient_bar = nicecolorbar, output = "figure11/PA_colobar/")

# quantile(metricsdb_precision$oldtarget_exp01_PRECISION, na.rm=TRUE, probs=c(0.25, 0.75))
# median(metricsdb_precision$oldtarget_exp01_PRECISION, na.rm=TRUE)



# 9. Table 6 ------------------------------------------

## EXPERIMENT 01
metricsdb_precision <- metricsdb %>% dplyr::select(matches('PRECISION')) %>% dplyr::select(matches('exp01'))
metricsdb_recall <- metricsdb %>% dplyr::select(matches('RECALL')) %>% dplyr::select(matches('exp01'))
metricsdb_boa <- metricsdb %>% dplyr::select(matches('BOA')) %>% dplyr::select(matches('exp01'))

gen_table("oldtarget_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("unet_mobilenetv2_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("kappamaskl2a_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("kappamaskl1c_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("fmask_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("s2cloudless_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("sen2cor_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("qa60_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("CDFCNN_RGBI_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("CDFCNN_RGBISWIR_exp01", metricsdb_recall, metricsdb_precision, metricsdb_boa)


## EXPERIMENT 02
metricsdb_precision <- metricsdb %>% dplyr::select(matches('PRECISION')) %>% dplyr::select(matches('exp02'))
metricsdb_recall <- metricsdb %>% dplyr::select(matches('RECALL')) %>% dplyr::select(matches('exp02'))
metricsdb_boa <- metricsdb %>% dplyr::select(matches('BOA')) %>% dplyr::select(matches('exp02'))

gen_table("oldtarget_exp02", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("unet_mobilenetv2_exp02", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("kappamaskl2a_exp02", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("kappamaskl1c_exp02", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("sen2cor_exp02", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("fmask_exp02", metricsdb_recall, metricsdb_precision, metricsdb_boa)


## EXPERIMENT 03
metricsdb_precision <- metricsdb %>% dplyr::select(matches('PRECISION')) %>% dplyr::select(matches('exp03'))
metricsdb_recall <- metricsdb %>% dplyr::select(matches('RECALL')) %>% dplyr::select(matches('exp03'))
metricsdb_boa <- metricsdb %>% dplyr::select(matches('BOA')) %>% dplyr::select(matches('exp03'))

gen_table("oldtarget_exp03", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("unet_mobilenetv2_exp03", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("kappamaskl2a_exp03", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("kappamaskl1c_exp03", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("sen2cor_exp03", metricsdb_recall, metricsdb_precision, metricsdb_boa)
gen_table("fmask_exp03", metricsdb_recall, metricsdb_precision, metricsdb_boa)
