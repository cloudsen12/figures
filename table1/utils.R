countlabeling <- function(dataset, path) {
  if (dataset == "irish") {
    # list of scenes
    list.img <-
      list.files(
        path,
        pattern = "LE07",
        recursive = T,
        full.names = T
      )
    # id of scenes
    id <-
      list.img %>%
      basename() %>%
      str_sub(1, -5)
    # build empty dataframe
    df <-
      tibble(
        p0 = numeric(),
        p64 = numeric(),
        p128 = numeric(),
        p192 = numeric(),
        p255 = numeric(),
        id = character(),
      )
    # fill table
    for (i in seq_along(list.img)) {
      cat(sprintf("scene = %1s\n", i))
      options(warn = -1)
      ee_img <- ee$Image(sprintf("LANDSAT/LE07/C01/T2_TOA/%1$s", id[i]))
      # if image not exist, runs it
      if (
        grepl(
          try(
            ee_get_date_img(ee_img),
            silent = TRUE
          ),
          pattern = "Error"
        ) %>% unique()
      ) {
        ee_img <- ee$Image(sprintf("LANDSAT/LE07/C01/T1/%1$s", id[i]))
      }
      # load raster
      img <- raster(list.img[i])
      # load tile
      tile <-
        ee_img$
          geometry() %>%
        ee_as_sf() %>%
        st_transform(crs = st_crs(img)) %>%
        sf::st_buffer(-2500)
      # mask raster
      img %<>% raster::mask(mask = tile)
      # build table
      df %<>%
        bind_rows(
          t(
            data.frame(
              getValues(img) %>%
                table()
            )
          ) %>%
            as_data_frame() %>%
            janitor::row_to_names(row_number = 1) %>%
            rename_all(~ sprintf("p%1s", .x)) %>%
            mutate_all(as.numeric) %>%
            mutate(
              valid = rowSums(
                across(where(is.numeric)),
                na.rm = T
              ),
              invalid = ncell(img) - valid,
              total = ncell(img)
            ) %>%
            mutate(id = id[i])
        )
    }
  } else if (dataset == "sparcs") {
    # List of raster
    lst <-
      list.files(
        path,
        pattern = "\\mask.png$",
        full.names = T,
        recursive = T
      )
    # List of scene id
    id <- basename(lst) %>% str_sub(1, -5)
    # Build table ----
    # create empty table
    df <-
      tibble(
        p0 = numeric(),
        p1 = numeric(),
        p2 = numeric(),
        p3 = numeric(),
        p4 = numeric(),
        p5 = numeric(),
        p6 = numeric(),
        id = character()
      )
    # fill table
    for (i in seq_along(lst)) {
      cat(sprintf("scene = %1s\n", i))
      # load raster
      img <- raster(lst[i])
      # build table
      df %<>%
        bind_rows(
          t(
            data.frame(
              getValues(img) %>%
                table()
            )
          ) %>%
            as_tibble() %>%
            janitor::row_to_names(row_number = 1) %>%
            rename_all(~ sprintf("p%1s", .x)) %>%
            mutate_all(as.numeric) %>%
            mutate(
              valid = rowSums(
                across(where(is.numeric)),
                na.rm = T
              ),
              invalid = ncell(img) - valid,
              total = ncell(img)
            ) %>%
            mutate(id = id[i])
        )
    }
  } else if (dataset == "s2_hollstein") {
    path <- "dataset/s2hollstein/"
    # Load dataset
    img <-
      list.files(
        path, pattern = "\\.h5$",
        full.names = T
      ) %>%
      h5dump()
    # Build table
    df <-
      t(
        table(img$classes) %>% as.matrix()
      ) %>%
      as_tibble() %>%
      mutate(
        valid = rowSums(
          across(where(is.numeric)),
          na.rm = T
        ),
        invalid = 0,
        total = valid
      ) %>%
      mutate(id = list.files(path, pattern = "\\.h5$"))
  }
  # change colnames
  if (dataset == "irish") {
    names(df) <-
      c(
        "Fill", "Cloud_Shadow", "Clear", "Thin_Cloud",
        "Cloud", "id", "Valid", "Invalid", "Total"
      )
  } else if (dataset == "sparcs") {
    names(df) <-
      c(
        "Shadow", "Shadow_over_Water", "Water",
        "Snow", "Land", "Cloud", "Flooded", "id",
        "Valid", "Invalid", "Total"
      )
  } else if (dataset == "s2_hollstein") {
    names(df) <-
      c(
        "Cloud", "Cirrus", "Snow",
        "Shadow", "Water", "Clear_sky",
        "Valid", "Invalid", "Total", "id"
      )
  }
  # return table
  mutate_all(df, ~ replace(., is.na(.), 0)) %>% return()
}