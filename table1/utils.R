countlabeling <- function(dataset, path) {
  if (dataset == "irish") {
    # list of scenes
    lst <-
      list.files(
        path,
        pattern = "LE07",
        recursive = T,
        full.names = T
      )
    # id of scenes
    id <-
      lst %>%
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
    for (i in seq_along(lst)) {
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
      img <- raster(lst[i])
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
    # Build table
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
    # Load dataset
    img <-
      list.files(
        path,
        pattern = "\\.h5$",
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
  } else if (dataset == "baetens_hagolle") {
    # List of raster
    lst <-
      list.files(
        path,
        pattern = "\\map.tif$",
        full.names = T,
        recursive = T
      )
    # List of scene id
    id <-
      c(
        list.dirs(
          sprintf("%1s/Hollstein", path),
          recursive = F,
          full.names = F
        ),
        list.dirs(
          sprintf("%1s/Reference_dataset", path),
          recursive = F,
          full.names = F
        )
      )
    # Build table
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
        p7 = numeric(),
        id = character()
      )
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
  } else if (dataset == "cloud_catalog") {
    np <- import("numpy")
    # List of raster
    lst <-
      list.files(
        path,
        pattern = "\\.npy$",
        full.names = T,
        recursive = T
      )
    # Build table
    # create empty table
    df <-
      tibble(
        p1 = numeric(),
        p2 = numeric(),
        p3 = numeric(),
        id = character()
      )
    # fill table
    for (i in seq_along(lst)) {
      cat(sprintf("scene = %1s\n", i))
      # load raster
      img <- sum(brick(np$load(lst[i])) * c(1, 2, 3))
      # List of scene id
      id <- basename(lst[i]) %>% str_sub(1, -5)
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
            mutate(id = id)
        )
    }
  } else if (dataset == "biome8") {
    # List of raster
    lst <-
      list.files(
        path,
        pattern = "\\.img$",
        full.names = T,
        recursive = T
      )
    # Build table
    # create empty table
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
    for (i in seq_along(lst)[1:5]) {
      cat(sprintf("scene = %1s\n", i))
      # load raster
      img <- raster(lst[i])
      # List of scene id
      id <- basename(lst[i]) %>% str_sub(1, -15)
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
            mutate(id = id)
        )
    }
  } else if (dataset == "38cloud") {
    # load list of scene
    lst <-
      list.files(
        sprintf("%1$s/entire_scene_gts", path),
        pattern = "\\.TIF$", full.names = T
      )
    # load list of metadata
    ls.mtl <-
      list.files(
        sprintf("%1$s/metadata", path),
        pattern = "\\MTL.txt$", full.names = T
      )
    # list of id by scene
    id <-
      basename(lst) %>%
      str_sub(-34, -20) %>%
      sprintf(fmt = "LC08_%1s")
    # Build table
    # create empty table
    df <-
      tibble(
        p0 = numeric(),
        p1 = numeric(),
        id = character()
      )
    # fill table
    for (i in seq_along(lst)) {
      cat(sprintf("scene = %1s\n", i))
      # load ee image object
      ee_img <- ee$Image(sprintf("LANDSAT/LC08/C01/T1/%1s", id[i]))
      # get epsg
      epsg <-
        ee_img$getInfo()$bands[[12]][[4]] %>%
        str_sub(-5, -1) %>%
        as.numeric() %>%
        sprintf(fmt = "+init=epsg:%1s")
      # get coordinates list of tile with buffer
      tile <-
        ee_as_sf(ee_img$select("B1")$geometry()) %>%
        sf::st_transform(crs = epsg) %>%
        sf::st_buffer(-2750)
      # read metadata
      mtl <- read_lines(ls.mtl[i])
      # extract corners
      corners <-
        c(
          "CORNER_UL_PROJECTION_X_PRODUCT",
          "CORNER_UR_PROJECTION_X_PRODUCT",
          "CORNER_LL_PROJECTION_Y_PRODUCT",
          "CORNER_UL_PROJECTION_Y_PRODUCT"
        )
      # build georeferenced raster and masking
      img <-
        raster(
          raster(lst[i]) %>% as.matrix(),
          xmn = mtl[grep(mtl, pattern = corners[1])] %>% parse_number(),
          xmx = mtl[grep(mtl, pattern = corners[2])] %>% parse_number(),
          ymn = mtl[grep(mtl, pattern = corners[3])] %>% parse_number(),
          ymx = mtl[grep(mtl, pattern = corners[4])] %>% parse_number(),
          crs = st_crs(tile)$input
        ) %>%
        raster::mask(tile)
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
  } else if (dataset == "95cloud") {
    path <- "dataset/95cloud"
    # load list of scene
    lst <-
      list.files(
        sprintf("%1$s/entire_scene_gts", path),
        pattern = "\\.TIF$", full.names = T
      )
    # load list of metadata
    ls.mtl <-
      list.files(
        sprintf("%1$s/metadata", path),
        pattern = "\\MTL.txt$", full.names = T
      )
    # list of id by scene
    id <-
      basename(lst) %>%
      str_sub(-34, -20) %>%
      sprintf(fmt = "LC08_%1s")
    # Build table
    # create empty table
    df <-
      tibble(
        p0 = numeric(),
        p1 = numeric(),
        id = character()
      )
    # fill table
    for (i in seq_along(lst)) {
      cat(sprintf("scene = %1s\n", i))
      # load ee image object
      ee_img <- ee$Image(sprintf("LANDSAT/LC08/C01/T1/%1s", id[i]))
      # get epsg
      epsg <-
        ee_img$getInfo()$bands[[12]][[4]] %>%
        str_sub(-5, -1) %>%
        as.numeric() %>%
        sprintf(fmt = "+init=epsg:%1s")
      # get coordinates list of tile with buffer
      tile <-
        ee_as_sf(ee_img$select("B1")$geometry()) %>%
        sf::st_transform(crs = epsg) %>%
        sf::st_buffer(-2750)
      # read metadata
      mtl <- read_lines(ls.mtl[i])
      # extract corners
      corners <-
        c(
          "CORNER_UL_PROJECTION_X_PRODUCT",
          "CORNER_UR_PROJECTION_X_PRODUCT",
          "CORNER_LL_PROJECTION_Y_PRODUCT",
          "CORNER_UL_PROJECTION_Y_PRODUCT"
        )
      # build georeferenced raster and masking
      img <-
        raster(
          raster(lst[i]) %>% as.matrix(),
          xmn = mtl[grep(mtl, pattern = corners[1])] %>% parse_number(),
          xmx = mtl[grep(mtl, pattern = corners[2])] %>% parse_number(),
          ymn = mtl[grep(mtl, pattern = corners[3])] %>% parse_number(),
          ymx = mtl[grep(mtl, pattern = corners[4])] %>% parse_number(),
          crs = st_crs(tile)$input
        ) %>%
        raster::mask(tile)
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
  } else if (dataset == "baetens_hagolle") {
    names(df) <-
      c(
        "no_data", "not_used", "low_clouds", "high_clouds",
        "clouds_shadows", "land", "water", "snow", "id",
        "Valid", "Invalid", "Total"
      )
  } else if (dataset == "cloud_catalog") {
    names(df) <-
      c(
        "Clear", "Cloud", "Cloud_Shadow",
        "id", "Valid", "Invalid", "Total"
      )
  } else if (dataset == "biome8") {
    names(df) <-
      c(
        "Fill", "Cloud_Shadow", "Clear",
        "Thin_Cloud", "Cloud", "id",
        "Valid", "Invalid", "Total"
      )
  } else if (dataset == "38cloud") {
    names(df) <- c("Cloud", "Clear", "id", "Valid", "Invalid", "Total")
  } else if (dataset == "95cloud") {
    names(df) <- c("Cloud", "Clear", "id", "Valid", "Invalid", "Total")
  }
  # return table
  mutate_all(df, ~ replace(., is.na(.), 0)) %>% return()
}