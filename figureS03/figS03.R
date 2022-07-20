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
library(ggplot2)

# 2. Create color pal -----------------------------------------------------
cloudsen12_metadata <- read_csv("data/cloudsen12_metadata.csv")
results <- table(cloudsen12_metadata[cloudsen12_metadata$test == 1,]$s2_sen2cor_version)

sen2cor_version <- tibble(
  name = c("Sen2Corv.02.06.03", "Sen2Corv.02.06.06", "Sen2Corv.02.07.01", "Sen2Corv.02.08.00"),
  n_imgs = c(sum(results[1:2]), sum(results[3:4]), as.numeric(results[5]), sum(results[6:8]))
)

ggplot(sen2cor_version, aes(x=name, y=n_imgs)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  xlab("") +
  ylab("# IPs in test")

