# Removing all objects including loaded libraries
rm(list = ls(all = TRUE))

# Installing and loading packages
if (!require("pacman")) {
  install.packages("pacman")
}
# Unloading all package except base
pacman::p_unload(
  pacman::p_loaded(),
  character.only = TRUE
)
# Loading packages
pacman::p_load(tidyverse, magick, Hmisc)

# Build data before plot
other.dset <-
  read_csv("data/dataset.csv") %>%
  dplyr::filter(Dataset %nin% "cloudSEN12") %>%
  arrange(pixels) %>%
  mutate(
    pixels = round(pixels / 10^6, 1),
    lab = sprintf(
      "%1s%2s",
      round(pixels * c(rep(1, 3), rep(10^-3, 4)), 1),
      c(rep("", 3), rep("x10³", 4))
    ),
    y = pixels + 2000
  )

dset <-
  other.dset %>%
  bind_rows(
    read_csv("data/dataset.csv") %>%
      dplyr::filter(Dataset %in% "cloudSEN12") %>%
      mutate(id = c(3, 1, 2)) %>%
      arrange(desc(id)) %>%
      mutate(
        pixels = round(pixels / 10^6, 1),
        lab = sprintf("%1sx10³", round(pixels / 10^3, 1)),
        ycum = cumsum(pixels),
        y = ycum - (pixels / 2)
      ) %>%
      arrange(id)
  ) %>%
  arrange(desc(row_number())) %>%
  mutate(Dataset = as_factor(Dataset))

# Plot data
plot <-
  ggplot(dset, aes(x = Dataset, y = pixels, fill = type, group = sensor)) +
  labs(x = NULL, y = "\n# of pixels [mill.]") +
  geom_bar(position = "stack", stat = "identity", colour = "black") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(
    breaks = seq(0, 15000, 3000),
    limits = c(0, 15000)
  ) +
  theme_bw() +
  theme(
    legend.margin = margin(3, 7, 7, 7),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(.5, "cm"),
    legend.title = element_blank(),
    legend.position = c(.67, .88),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 12, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 10, colour = "black",
      family = "Source Sans Pro",
      angle = 0, vjust = .6 # , face = "bold"
    ),
    axis.text.y = element_text(
      size = 12, color = "black",
      family = "Source Sans Pro",
      angle = 0, vjust = .6 # , face = "bold"
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
    size = c(rep(3, 3), rep(4, 7)),
    angle = c(rep(0, 3), rep(0, 7)),
    color = "black", family = "Source Sans Pro", fontface = "bold"
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_flip()

# Save ggplot object
ggsave(
  plot = plot, "exports/barplot.png",
  width = 13, height = 13, units = "cm", dpi = 500
)

#' Trim figure
img <-
  magick::image_read("exports/barplot.png", strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' Save figure
image_write(img, path = "exports/barplot.png", format = "png")