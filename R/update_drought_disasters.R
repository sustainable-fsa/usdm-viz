update_drought_disasters <-
  function(
    out_dir = file.path("docs","disasters")
    ){

    dir.create(out_dir,
               recursive = TRUE,
               showWarnings = FALSE)

    latest_secretarial <-
      jsonlite::fromJSON(
        "https://data.sustainable-fsa.com/fsa-disasters/manifest.json"
      ) |>
      dplyr::filter(stringr::str_detect(path, "_SEC_")) %$%
      mtime |>
      max() |>
      lubridate::as_date()
    
    year <-
      latest_secretarial %>%
      lubridate::as_date() %>%
      lubridate::year()
    
    disasters <-
      "https://data.sustainable-fsa.com/fsa-disasters/fsa-disasters.parquet" |>
      # "fsa-disasters.parquet" |>
      arrow::read_parquet() |>
      dplyr::filter(`Designation/Declaration Type` == "Secretarial",
                    stringr::str_detect(`Disaster Type`, "DROUGHT"),
                    `Disaster Year` == year) |>
      dplyr::arrange(`Designation Code`) %>%
      dplyr::distinct(FIPS, .keep_all = TRUE)
    
    disasters <-
      disasters %>%
      dplyr::left_join(
        get_oconus(layer = "counties", 
                   year = year - 1) |>
          dplyr::mutate(FIPS = paste0(state, county))
        ) %>%
      sf::st_as_sf()
    
    p <-
      ggplot(get_oconus(layer = "oconus",
                        year = year - 1)) +
      geom_sf(data = get_oconus(layer = "oconus",
                                year = year - 1),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(data = disasters,
              aes(fill = `Designation Code`),
              color = "white",
              size = 0.05,
              show.legend = T) +
      geom_sf(
        data = get_oconus(
          layer = "counties",
          year = year - 1
        ) %>%
          rmapshaper::ms_innerlines() %>%
          sf::st_cast("MULTILINESTRING"),
        color = "white",
        # alpha = 0,
        show.legend = FALSE,
        linewidth = 0.1) +
      geom_sf(data = get_oconus(
        layer = "states",
        year = year - 1
      ) %>%
        rmapshaper::ms_innerlines() %>%
        sf::st_cast("MULTILINESTRING"),
      color = "white",
      # alpha = 0,
      show.legend = FALSE,
      linewidth = 0.2) +
      scale_fill_manual(
        values = 
          c(
            "Primary" = "#DC0005",
            "Contiguous" = "#FD9A09"
          ),
        na.value = NA,
        drop = FALSE,
        na.translate = FALSE,
        name = paste0(year, " USDA Secretarial\nDisaster Designations\nfor Drought"),
        guide = guide_legend(direction = "vertical",
                             title.position = "top",
                             ncol = 1) ) +
      usdm_layout(attribution = "The Secretary of Agriculture is authorized to designate counties\nas disaster areas for emergency loan and assistance programs,\nsuch as Farm Service Agency (FSA) disaster assistance programs.\nMap data courtesy of the FSA. Map courtesy of the Montana Climate Office.",
                  footnote = paste0("Data updated ", format(lubridate::as_date(latest_secretarial), "%B %e, %Y")) %>% stringr::str_squish())
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    grid::grid.draw(gt) %>%
      ggsave(plot = .,
             filename = file.path(out_dir, "latest.png"),
             device = ragg::agg_png,
             width = 10,
             height = 5.14,
             # height = 6.86,
             bg = "white",
             dpi = 600)
    
    return(
      list.files(
        out_dir,
        full.names = TRUE,
        recursive = TRUE,
        pattern = "latest"
      )
    )
    
  }
