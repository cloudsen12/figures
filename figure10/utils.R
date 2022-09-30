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
generate_density_gradient <- function(db, gradient_bar, xtext = "PA %") {
  db$model <- factor(db$model, c(
    "Human-level", "U-Net MobileNetV2", "KappaMask L1C", "KappaMask L2A",
    "Fmask", "s2cloudless", "Sen2Cor","QA60",
    "CD-FCNN RGBI", "CD-FCNN RGBISWIR"
  ) %>% rev())


  ggplot(db, aes(x = error, y = model, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantile_lines = TRUE, alpha = 0.5) +
    scale_fill_gradientn(colours = gradient_bar)+
    #geom_vline(xintercept = 0, linetype="longdash", color = "red", size=0.6) +
    theme_classic() +
    theme(
      axis.text.x = element_text(
        size = 11, colour = "black",
        family = "Source Sans Pro",
        angle = 0.5, vjust = .6
      ),
      axis.text.y = element_text(
        size = 12, color = "black",
        family = "Source Sans Pro",
        vjust = .6,
        hjust = 1
      ),
      axis.title.x = element_text(
        size = 12, color = "black",
        family = "Source Sans Pro", face = "bold"
      ),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black")
    ) +
    ylab("") +
    xlab(xtext) +
    theme(legend.position = "None")
}


metric_to_dprob <- function(db, gradient_bar, type = "fmask_RECALL") {
  # Subset the recall vector
  recall_t <- db[[type]]
  relative_frecuency <- hist(recall_t, seq(0,1, length.out = 101))$counts/length(na.omit(recall_t))

  # Define constant colors
  color_ref <- data.frame(
    freq = seq(0, 0.05, length.out = 100),
    color = gradient_bar
  )

  vector_container <- list()
  for (index in 1:100) {
    colort <- color_ref[which.min(abs(relative_frecuency[index] - color_ref$freq)),]$color
    vector_container[index] <- colort
  }

  # Create colo ramp
  finalcolor <- unlist(vector_container)

  # Create a matrix color
  mx1 <- image(
    x = matrix(1:5000),
    col = colorRampPalette(colorRampPalette(finalcolor)(100))(1000),
    axes = F
  )
}

generate_png <- function(db, gradient_bar, output = "figure10/PA_colobar/") {
  dir.create(output, showWarnings = FALSE)
  coln <- colnames(db)
  for (index in 1:10) {
    #index <- 8
    ffg <- sprintf("%s/%s.png",output, coln[index])
    png(ffg)
    metric_to_dprob(db = db, type = coln[index], gradient_bar = gradient_bar)
    dev.off()
    mgk1 <- magick::image_read(ffg) %>%
      magick::image_trim()
    mgk1 %>% magick::image_crop("391x20") %>%
      magick::image_write(path = ffg, format = "png")
  }
}



# functions to generate cloud percentages ------------------------------

## OLD TARGET ---------------------------------------------------
oldtarget_cloudmask <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 1) + (cmsk == 2) > 0
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


oldtarget_cloudshadowmask <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 3)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

oldtarget_contaminated <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 1) + (cmsk == 2) + (cmsk == 3) > 0
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

## SEN2COR ------------------------------------------------------
### Generate CLOUD percentage sen2cor
sen2cor_cloudmask <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- cmsk == 8 | cmsk == 9 | cmsk == 10
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

## Generate CLOUD SHADOW percentage sen2cor
sen2cor_cloudshadow <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- cmsk == 3
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

## Generate CONTAMINATED percentage sen2cor
sen2cor_contaminated <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- cmsk == 8 | cmsk == 9 | cmsk == 10 | cmsk == 3
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}



## KAPPAMASK ------------------------------------------------------
### Generate CLOUD percentage KAPPAMASK
kappamask_cloudmask <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 3) | (cmsk == 4)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

### Generate CLOUD SHADOW percentage KAPPAMASK
kappamask_cloudshadow <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 2)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


### Generate CONTAMINATED percentage KAPPAMASK
kappamask_contaminated <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 3) | (cmsk == 4) | (cmsk == 2)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


## FMASK ------------------------------------------------------
### Generate CLOUD percentage FMASK
fmask_cloudmask <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 4)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

### Generate CLOUDSHADOW percentage FMASK
fmask_cloudshadow <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 2)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

### Generate CONTAMINATED percentage FMASK
fmask_contaminated <- function(image, actual) {
  cmsk <- read_stars(image)
  pd_cloud <- (cmsk == 2) | (cmsk == 4)
  predicted <- pd_cloud[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


## s2cloudless ------------------------------------------------------
### Generate CLOUD percentage s2cloudless
s2cloudless_cloudmask <- function(image, actual) {
  cmsk <- read_stars(image)
  arr_new <- array(NA, dim=c(1, 509, 509))
  arr_new[1,,] <- cmsk[[1]]/100
  predicted <- s2cloudless_cprob$get_mask_from_prob(arr_new) %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


## QA60 ------------------------------------------------------
### Generate CLOUD percentage QA60
q60_cloudmask <- function(image, actual) {
  cmsk <- read_stars(image)
  predicted <- (cmsk > 0)[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


## QA60 ------------------------------------------------------
### Generate CLOUD percentage QA60
CDFCNN_cloudmask <- function(image, actual) {
  predicted <- (read_stars(image) > 5000)[[1]] %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}



## OLD TARGET ---------------------------------------------------
unetmobilenetv2_cloudmask <- function(image, actual) {
  cmsk <- image
  pd_cloud <- (cmsk == 1) + (cmsk == 2) > 0
  predicted <- pd_cloud %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

unetmobilenetv2_cloudshadowmask <- function(image, actual) {
  cmsk <- image
  pd_cloud <- (cmsk == 3)
  predicted <- pd_cloud %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}

unetmobilenetv2_contaminated <- function(image, actual) {
  cmsk <- image
  pd_cloud <- (cmsk == 1) + (cmsk == 2) + (cmsk == 3) > 0
  predicted <- pd_cloud %>% as.numeric()

  # metrics --------------------------------------------------
  recall <- Metrics::recall(actual, predicted)
  precision <- Metrics::precision(actual, predicted)
  BOA <- 0.5*(recall + Metrics::recall(!actual, !predicted))
  c(recall, precision, BOA)
}


gen_table <- function(model = "Sen2Cor", metricsdb_recall, metricsdb_precision, metricsdb_boa) {
  cd_recall <- na.omit(metricsdb_recall[[paste0(model, "_RECALL")]])
  cd_pre <- na.omit(metricsdb_precision[[paste0(model, "_PRECISION")]])
  cd_BOA <- median(na.omit(metricsdb_boa[[paste0(model, "_BOA")]]))
  results <- c(
    cd_BOA,
    sum(cd_recall < 0.1)/length(cd_recall)*100,
    sum(cd_recall >= 0.1 & cd_recall <= 0.9)/length(cd_recall)*100,
    sum(cd_recall > 0.9)/length(cd_recall)*100,
    sum(cd_pre < 0.1)/length(cd_pre)*100,
    sum(cd_pre >= 0.1 & cd_pre <= 0.9)/length(cd_pre)*100,
    sum(cd_pre > 0.9)/length(cd_pre)*100
    #sum(sen2cor_boa < 0.6)/length(sen2cor_boa)*100,
    #sum(sen2cor_boa >= 0.6 & sen2cor_boa <= 0.9)/length(sen2cor_boa)*100,
    #sum(sen2cor_boa > 0.9)/length(sen2cor_boa)*100
  )
  sprintf("%s \\ \\cline{2-9}", paste0(round(results, 2), collapse = " & "))
}
