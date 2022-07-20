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
library(reticulate)
library(tidyverse)
library(stars)
library(sf)
source("figure11/utils.R")
# 2. Make predictions with the pretrained model (U-Net MobileNetV2)
source_python("figure11/unet_mobilenetv2_prediction/main.py")


# 3. Convert s2cloudless cloud probabilities to binary cloud mask ---------
s2cloudless <- import("s2cloudless")
s2cloudless_cprob <- s2cloudless$S2PixelCloudDetector()


# 4. Load metadata --------------------------------------------------------
METADATA_FILE <- "data/cloudsen12_metadata.csv"
cloudsen12_metadata <- read_csv(METADATA_FILE)
cloudsen12_metadata <- cloudsen12_metadata[cloudsen12_metadata$test == 1,]

# 5. Load labels -- (needs a connection to the cloudSEN12db) --------------
CLOUDSEN12_FOLDER <- "/media/csaybar/Elements SE/cloudSEN12/"
CLOUDSEN12_OLD <- "/media/csaybar/58059B472A3AA231/comparison/old/"

# 5. RUN iterativately for the test dataset
container_db <- list()
for (index in 1:nrow(cloudsen12_metadata)) {
  print(index)
  ## 5.1 Select a cloudSEN12 IP path
  image_id <- sprintf(
    fmt = "%s/high/%s/%s/labels/",
    CLOUDSEN12_FOLDER,
    cloudsen12_metadata$roi_id[index],
    cloudsen12_metadata$s2_id_gee[index]
  )
  S2_image <- sprintf(
    fmt = "%s/high/%s/%s/S2L1C.tif",
    CLOUDSEN12_FOLDER,
    cloudsen12_metadata$roi_id[index],
    cloudsen12_metadata$s2_id_gee[index]
  )

  ## 5.2 Generate reference for each experiments
  target <- read_stars(paste0(image_id, "manual_hq.tif"))
  target_CLOUD <- (target == 1) + (target == 2) > 0                         # Experiment 01
  target_CLOUDSHADOW <- target == 3                                         # Experiment 02
  target_CONTAMINATED <- (target == 1) + (target == 2) + (target == 3) > 0  # Experiment 03
  CLOUD_COVER_METADATA <- sum(target_CLOUD[[1]])/(509*509)


  ## 5.3 Manual label without control quality
  ### Download the data here: https://drive.google.com/file/d/1vkXeTfWW5Jbro_dXrH8FACdySNhIh2_9
  old_target <- sprintf(
    fmt = "%s/%s__%s__manual.tif",
    CLOUDSEN12_OLD,
    cloudsen12_metadata$roi_id[index],
    cloudsen12_metadata$s2_id_gee[index]
  )

  ### OLDTARGET - Cloud masking
  oldtarget_cloud_results <- oldtarget_cloudmask(
    image = old_target,
    actual = target_CLOUD[[1]]
  )

  ### OLDTARGET - Cloud Shadow masking
  oldtarget_cloudshadow_results <- oldtarget_cloudshadowmask(
    image = old_target,
    actual = target_CLOUDSHADOW[[1]]
  )

  ### OLDTARGET - Contaminated masking
  oldtarget_contaminated_results <- oldtarget_contaminated(
    image = old_target,
    actual = target_CONTAMINATED[[1]]
  )

  ## 5.4 SEN2COR
  ### SEN2COR - Cloud masking
  sen2cor_cloud_results <- sen2cor_cloudmask(
    image = paste0(image_id, "sen2cor.tif"),
    actual = target_CLOUD[[1]]
  )

  ### SEN2COR - Cloud shadow masking
  sen2cor_cloudshadow_results <- sen2cor_cloudshadow(
    image = paste0(image_id, "sen2cor.tif"),
    actual = target_CLOUDSHADOW[[1]]
  )

  ### SEN2COR - Contaminated masking
  sen2cor_contaminated_results <- sen2cor_contaminated(
    image = paste0(image_id, "sen2cor.tif"),
    actual = target_CONTAMINATED[[1]]
  )

  ## 5.5 KappaMask L1C
  ### KappaMask - Cloud masking
  kappamaskl1c_cloud_results <- kappamask_cloudmask(
    image = paste0(image_id, "kappamask_L1C.tif"),
    actual = target_CLOUD[[1]]
  )

  ### KappaMask - Cloud shadow masking
  kappamaskl1c_cloudshadow_results <- kappamask_cloudshadow(
    image = paste0(image_id, "kappamask_L1C.tif"),
    actual = target_CLOUDSHADOW[[1]]
  )

  ### KappaMask - Contaminated masking
  kappamaskl1c_contaminated_results <- kappamask_contaminated(
    image = paste0(image_id, "kappamask_L1C.tif"),
    actual = target_CONTAMINATED[[1]]
  )

  ## 5.6 KappaMask L2A
  ### KappaMask - Cloud masking
  kappamaskl2a_cloud_results <- kappamask_cloudmask(
    image = paste0(image_id, "kappamask_L2A.tif"),
    actual = target_CLOUD[[1]]
  )

  ### KappaMask - Cloud shadow masking
  kappamaskl2a_cloudshadow_results <- kappamask_cloudshadow(
    image = paste0(image_id, "kappamask_L2A.tif"),
    actual = target_CLOUDSHADOW[[1]]
  )

  ### KappaMask - Contaminated masking
  kappamaskl2a_contaminated_results <- kappamask_contaminated(
    image = paste0(image_id, "kappamask_L2A.tif"),
    actual = target_CONTAMINATED[[1]]
  )

  ## 5.7 Fmask
  ### Fmask - Cloud masking
  fmask_cloud_results <- fmask_cloudmask(
    image = paste0(image_id, "fmask.tif"),
    actual = target_CLOUD[[1]]
  )

  ### Fmask - Cloud shadow masking
  fmask_cloudshadow_results <- fmask_cloudshadow(
    image = paste0(image_id, "fmask.tif"),
    actual = target_CLOUDSHADOW[[1]]
  )

  ### Fmask - Contaminated masking
  fmask_contaminated_results <- fmask_contaminated(
    image = paste0(image_id, "fmask.tif"),
    actual = target_CONTAMINATED[[1]]
  )

  ## 5.8 s2cloudless
  ### s2cloudless - Cloud masking
  s2cloudless_cloud_results <- s2cloudless_cloudmask(
    image = paste0(image_id, "s2cloudless.tif"),
    actual = target_CLOUD[[1]]
  )

  ## 5.9 QA60
  ### qa60 - Cloud masking
  qa60_cloud_results <- q60_cloudmask(
    image = paste0(image_id, "qa60.tif"),
    actual = target_CLOUD[[1]]
  )

  ## 5.10 CD-FCNN RGBI
  ### CD-FCNN RGBI - Cloud masking
  CDFCNN_RGBI_cloud_results <- CDFCNN_cloudmask(
    image = paste0(image_id, "CD-FCNN-RGBI.tif"),
    actual = target_CLOUD[[1]]
  )

  ## 5.11 CD-FCNN RGBISWIR
  ### CD-FCNN RGBISWIR - Cloud masking
  CDFCNN_RGBISWIR_cloud_results <- CDFCNN_cloudmask(
    image = paste0(image_id, "CD-FCNN-RGBISWIR.tif"),
    actual = target_CLOUD[[1]]
  )

  ## 5.12 U-Net MobileNetV2
  unet_mobilenetv2_arr <- py$generate_cloud_prob(
    file = S2_image,
    pretrained = "figure11/unet_mobilenetv2_prediction/unet_mobilenetv2.ckpt"
  )
  unet_mobilenetv2_r <- raster(S2_image)
  unet_mobilenetv2_r[] <- unet_mobilenetv2_arr
  unet_mobilenetv2_r <- unet_mobilenetv2_r %>% stars::st_as_stars()

  ### U-Net MobileNetV2 - Cloud masking
  unet_mobilenetv2_cloud_results <- unetmobilenetv2_cloudmask(
    image = unet_mobilenetv2_r[[1]],
    actual = target_CLOUD[[1]]
  )

  ### U-Net MobileNetV2 - Cloud Shadow masking
  unet_mobilenetv2_cloudshadow_results <- unetmobilenetv2_cloudshadowmask(
    image = unet_mobilenetv2_r[[1]],
    actual = target_CLOUDSHADOW[[1]]
  )

  ### U-Net MobileNetV2 - Contaminated masking
  unet_mobilenetv2_contaminated_results <- unetmobilenetv2_contaminated(
    image = unet_mobilenetv2_r[[1]],
    actual = target_CONTAMINATED[[1]]
  )

  # 5.13 Put all together
  dt <- tibble(
    ROI_ID = cloudsen12_metadata$roi_id[index],
    S2_ID = cloudsen12_metadata$s2_id_gee[index],
    LUSE = cloudsen12_metadata$land_cover[index],
    CCOVER = CLOUD_COVER_METADATA,
    # OLDTARGET
    oldtarget_exp01_RECALL = oldtarget_cloud_results[1],
    oldtarget_exp01_PRECISION = oldtarget_cloud_results[2],
    oldtarget_exp01_BOA = oldtarget_cloud_results[3],
    oldtarget_exp02_RECALL = oldtarget_cloudshadow_results[1],
    oldtarget_exp02_PRECISION = oldtarget_cloudshadow_results[2],
    oldtarget_exp02_BOA = oldtarget_cloudshadow_results[3],
    oldtarget_exp03_RECALL = oldtarget_contaminated_results[1],
    oldtarget_exp03_PRECISION = oldtarget_contaminated_results[2],
    oldtarget_exp03_BOA = oldtarget_contaminated_results[3],
    # SEN2COR
    sen2cor_exp01_RECALL = sen2cor_cloud_results[1],
    sen2cor_exp01_PRECISION = sen2cor_cloud_results[2],
    sen2cor_exp01_BOA = sen2cor_cloud_results[3],
    sen2cor_exp02_RECALL = sen2cor_cloudshadow_results[1],
    sen2cor_exp02_PRECISION = sen2cor_cloudshadow_results[2],
    sen2cor_exp02_BOA = sen2cor_cloudshadow_results[3],
    sen2cor_exp03_RECALL = sen2cor_contaminated_results[1],
    sen2cor_exp03_PRECISION = sen2cor_contaminated_results[2],
    sen2cor_exp03_BOA = sen2cor_contaminated_results[3],
    # KAPPAMASK L1C
    kappamaskl1c_exp01_RECALL = kappamaskl1c_cloud_results[1],
    kappamaskl1c_exp01_PRECISION = kappamaskl1c_cloud_results[2],
    kappamaskl1c_exp01_BOA = kappamaskl1c_cloud_results[3],
    kappamaskl1c_exp02_RECALL = kappamaskl1c_cloudshadow_results[1],
    kappamaskl1c_exp02_PRECISION = kappamaskl1c_cloudshadow_results[2],
    kappamaskl1c_exp02_BOA = kappamaskl1c_cloudshadow_results[3],
    kappamaskl1c_exp03_RECALL = kappamaskl1c_contaminated_results[1],
    kappamaskl1c_exp03_PRECISION = kappamaskl1c_contaminated_results[2],
    kappamaskl1c_exp03_BOA = kappamaskl1c_contaminated_results[3],
    # KAPPAMASK L2A
    kappamaskl2a_exp01_RECALL = kappamaskl2a_cloud_results[1],
    kappamaskl2a_exp01_PRECISION = kappamaskl2a_cloud_results[2],
    kappamaskl2a_exp01_BOA = kappamaskl2a_cloud_results[3],
    kappamaskl2a_exp02_RECALL = kappamaskl2a_cloudshadow_results[1],
    kappamaskl2a_exp02_PRECISION = kappamaskl2a_cloudshadow_results[2],
    kappamaskl2a_exp02_BOA = kappamaskl2a_cloudshadow_results[3],
    kappamaskl2a_exp03_RECALL = kappamaskl2a_contaminated_results[1],
    kappamaskl2a_exp03_PRECISION = kappamaskl2a_contaminated_results[2],
    kappamaskl2a_exp03_BOA = kappamaskl2a_contaminated_results[3],
    # FMASK
    fmask_exp01_RECALL = fmask_cloud_results[1],
    fmask_exp01_PRECISION = fmask_cloud_results[2],
    fmask_exp01_BOA = fmask_cloud_results[3],
    fmask_exp02_RECALL = fmask_cloudshadow_results[1],
    fmask_exp02_PRECISION = fmask_cloudshadow_results[2],
    fmask_exp02_BOA = fmask_cloudshadow_results[3],
    fmask_exp03_RECALL = fmask_contaminated_results[1],
    fmask_exp03_PRECISION = fmask_contaminated_results[2],
    fmask_exp03_BOA = fmask_contaminated_results[3],
    # U-Net MobileNetV2
    unet_mobilenetv2_exp01_RECALL = unet_mobilenetv2_cloud_results[1],
    unet_mobilenetv2_exp01_PRECISION = unet_mobilenetv2_cloud_results[2],
    unet_mobilenetv2_exp01_BOA = unet_mobilenetv2_cloud_results[3],
    unet_mobilenetv2_exp02_RECALL = unet_mobilenetv2_cloudshadow_results[1],
    unet_mobilenetv2_exp02_PRECISION = unet_mobilenetv2_cloudshadow_results[2],
    unet_mobilenetv2_exp02_BOA = unet_mobilenetv2_cloudshadow_results[3],
    unet_mobilenetv2_exp03_RECALL = unet_mobilenetv2_contaminated_results[1],
    unet_mobilenetv2_exp03_PRECISION = unet_mobilenetv2_contaminated_results[2],
    unet_mobilenetv2_exp03_BOA = unet_mobilenetv2_contaminated_results[3],
    # S2Cloudless
    s2cloudless_exp01_RECALL = s2cloudless_cloud_results[1],
    s2cloudless_exp01_PRECISION = s2cloudless_cloud_results[2],
    s2cloudless_exp01_BOA = s2cloudless_cloud_results[3],
    # QA60
    qa60_exp01_RECALL = qa60_cloud_results[1],
    qa60_exp01_PRECISION = qa60_cloud_results[2],
    qa60_exp01_BOA = qa60_cloud_results[3],
    # CD-FCNN RGBI
    CDFCNN_RGBI_exp01_RECALL = CDFCNN_RGBI_cloud_results[1],
    CDFCNN_RGBI_exp01_PRECISION = CDFCNN_RGBI_cloud_results[2],
    CDFCNN_RGBI_exp01_BOA = CDFCNN_RGBI_cloud_results[3],
    # CD-FCNN RGBISWIR
    CDFCNN_RGBISWIR_exp01_RECALL = CDFCNN_RGBISWIR_cloud_results[1],
    CDFCNN_RGBISWIR_exp01_PRECISION = CDFCNN_RGBISWIR_cloud_results[2],
    CDFCNN_RGBISWIR_exp01_BOA = CDFCNN_RGBISWIR_cloud_results[3]
  )
  container_db[[index]] <- dt
}

# 6 Write the results
total_target <- bind_rows(container_db)
write_csv(total_target, "figure11/cloudsen12_metrics.csv")
