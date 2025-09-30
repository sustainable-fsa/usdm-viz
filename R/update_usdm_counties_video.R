# c(2000, 2010:(lubridate::year(lubridate::today()))) %>%
#   purrr::walk(
#     \(x){
#       tryCatch(get_oconus(year = x), error = function(e){"No download available"})
#     }
#   )

plot_usdm_county <-
  function(x,
           out_dir = file.path("data","usdm-counties")
  ){
    
    x %<>%
      lubridate::as_date() %>%
      format("%Y-%m-%d")
    
    year <-
      lubridate::as_date(x) %>%
      lubridate::year()
    
    year = dplyr::case_when(
      year <= 2010 ~ 2000, 
      year %in% c(2012, 2013) ~ 2010,
      .default = year - 1
    )
    
    outfile <- 
      file.path(out_dir, "png", paste0(x,".png"))
    
    if(file.exists(outfile))
      return(outfile)
    
    dir.create(dirname(outfile),
               recursive = TRUE,
               showWarnings = FALSE)
    
    usdm <-
      httr2::url_modify(
        "https://sustainable-fsa.github.io",
        path = file.path("usdm-counties",
                         "data",
                         "usdm",
                         paste0("USDM_",x,".parquet"))) %>%
      arrow::read_parquet() %>%
      dplyr::mutate(id = paste0(STATEFP, COUNTYFP)) %>%
      dplyr::group_by(id) %>%
      dplyr::filter(usdm_class == max(usdm_class)) %>%
      dplyr::ungroup() %>%
      dplyr::select(id, usdm_class) %>%
      dplyr::mutate(usdm_class = 
                      factor(usdm_class,
                             levels = paste0("D",0:4),
                             labels = c(
                               "Abnormally Dry: D0",
                               "Moderate Drought: D1",
                               "Severe Drought: D2",
                               "Extreme Drought: D3",
                               "Exceptional Drought: D4"
                             ),
                             ordered = TRUE
                      )
      ) %>%
      na.omit() %>%
      dplyr::left_join(
        get_oconus(
          layer = "counties",
          year = year
        ),
        by = "id") %>%
      sf::st_as_sf()
    
    p <-
      ggplot(get_oconus(layer = "oconus",
                        year = year)) +
      geom_sf(data = get_oconus(layer = "oconus",
                                year = year),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(aes(fill = usdm_class),
              data = usdm,
              color = "white",
              linewidth = NA,
              show.legend = TRUE) +
      geom_sf(
        data = get_oconus(
          layer = "counties",
          year = year
        ) %>%
          rmapshaper::ms_innerlines() %>%
          sf::st_cast("MULTILINESTRING"),
        color = "white",
        # alpha = 0,
        show.legend = FALSE,
        linewidth = 0.1) +
      geom_sf(data = get_oconus(
        layer = "states",
        year = year
      ) %>%
        rmapshaper::ms_innerlines() %>%
        sf::st_cast("MULTILINESTRING"),
      color = "white",
      # alpha = 0,
      show.legend = FALSE,
      linewidth = 0.2) +
      scale_fill_manual(
        values = c("#ffff00",
                   "#fcd37f",
                   "#ffaa00",
                   "#e60000",
                   "#730000"),
        drop = FALSE,
        name = paste0("US Drought Monitor\n",
                      format(lubridate::as_date(x), "%B %e, %Y") %>% 
                        stringr::str_squish()),
        guide = guide_legend(direction = "vertical",
                             title.position = "top",
                             override.aes = list(linewidth = 0.5)) 
      ) +
      usdm_layout(
        attribution = "The U.S. Drought Monitor is jointly produced by the National Drought\nMitigation Center at the University of Nebraska-Lincoln, the United States\nDepartment of Agriculture, and the National Oceanic and Atmospheric Administration.\nMap data courtesy of NDMC. Aggregation and map courtesy of the Montana Climate Office."
      )
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    grid::grid.draw(gt) %>%
      ggsave(plot = .,
             filename = outfile,
             device = ragg::agg_png,
             width = 10,
             height = 5.14,
             # height = 6.86,
             bg = "white",
             dpi = 300)
    
    
    return(outfile)
  }

update_usdm_counties_video <-
  function(as_of = lubridate::today(),
           out_dir = file.path("data", "usdm-counties")){
    plan(
      strategy = multisession, 
      workers = parallel::detectCores()
    )
    
    out <-
      get_usdm_dates(as_of = as_of) %>%
      furrr::future_map_chr(plot_usdm_county,
                            out_dir = out_dir)
    
    plan(sequential)
    
    invisible({
      list.files(file.path(out_dir, "png"),
                 full.names = TRUE,
                 pattern = "\\d") %>%
        sort() %>%
        dplyr::last() %>%
        file.copy(to = file.path("docs", "usdm-counties", "latest.png"),
                  overwrite = TRUE)
    })
    
    if(
      !file.exists(file.path("docs", "usdm-counties", "latest.mp4")) ||
      av::av_video_info(file.path("docs", "usdm-counties", "latest.mp4"))$video$frames <
      length(list.files(file.path(out_dir, "png")))
    ){
      system2(
        command = "ffmpeg",
        args = paste0(
          " -r 15",
          " -pattern_type glob -i '", file.path(out_dir, "png","[0-9]*.png'"),
          " -s:v 3000x1542",
          " -c:v libx265",
          " -crf 28",
          " -preset fast",
          " -tag:v hvc1",
          " -pix_fmt yuv420p10le",
          " -an",
          " ", file.path("docs", "usdm-counties", "latest.mp4"),
          " -y"),
        wait = TRUE
      )
      
      system2(
        command = "ffmpeg",
        args = paste0(
          "-i ", file.path("docs", "usdm-counties", "latest.mp4"),
          " -c:v libvpx-vp9",
          " -crf 30",
          " -b:v 0",
          " -row-mt 1",
          " ", file.path("docs", "usdm-counties", "latest.webm"),
          " -y"),
        wait = TRUE
      )
    }
    
    return(out)
  }
