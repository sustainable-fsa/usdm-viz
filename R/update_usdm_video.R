plot_usdm <-
  function(x,
           out_dir = file.path("data","usdm")){
    
    x %<>%
      lubridate::as_date() %>%
      format("%Y-%m-%d")
    
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
        path = file.path("usdm",
                         "usdm",
                         "data",
                         "parquet",
                         paste0("USDM_",x,".parquet"))) %>%
      sf::read_sf() %>%
      sf::st_cast("MULTIPOLYGON",
                  warn = FALSE) %>%
      sf::st_cast("POLYGON",
                  warn = FALSE) %>%
      tigris::shift_geometry() %>%
      sf::st_make_valid() %>%
      dplyr::group_by(date, usdm_class) %>%
      dplyr::summarise(.groups = "drop") %>%
      sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
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
      )
    
    p <-
      ggplot(get_oconus(layer = "oconus")) +
      geom_sf(data = get_oconus(layer = "oconus"),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(aes(fill = usdm_class),
              data = usdm,
              color = "white",
              linewidth = 0.1,
              show.legend = TRUE) +
      geom_sf(data = get_oconus(layer = "states") %>%
                rmapshaper::ms_innerlines() %>%
                sf::st_cast("MULTILINESTRING"),
              color = "white",
              # alpha = 0,
              show.legend = FALSE,
              linewidth = 0.2) +
      # geom_sf(data = usdm %>%
      #           rmapshaper::ms_innerlines() %>%
      #           sf::st_cast("MULTILINESTRING"),
      #         color = "white",
      #         # alpha = 0,
      #         show.legend = FALSE,
      #         linewidth = 0.1) +
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
      usdm_layout()
    
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

update_usdm_video <-
  function(as_of = lubridate::today(),
           out_dir = file.path("data", "usdm")){
    plan(
      strategy = multisession, 
      workers = parallel::detectCores()
    )
    
    out <-
      get_usdm_dates(as_of = as_of) %>%
      furrr::future_map_chr(plot_usdm,
                            out_dir = out_dir,
                            .options = furrr::furrr_options(seed = TRUE))
    
    plan(sequential)
    
    invisible({
      list.files(file.path(out_dir, "png"),
                 full.names = TRUE,
                 pattern = "\\d") %>%
        sort() %>%
        dplyr::last() %>%
        file.copy(to = file.path("docs", "usdm", "latest.png"),
                  overwrite = TRUE)
    })
    
    if(
      !file.exists(file.path("docs", "usdm", "latest.mp4")) ||
      av::av_video_info(file.path("docs", "usdm", "latest.mp4"))$video$frames <
      length(list.files(file.path(out_dir, "png")))
    ){
      system2(
        command = "ffmpeg",
        args = paste0(
          " -r 15",
          " -pattern_type glob -i '", file.path(out_dir, "png","[0-9]*.png'"),
          # " -s:v 3000x2058",
          " -s:v 3000x1542",
          " -c:v libx265",
          " -crf 28",
          " -preset fast",
          " -tag:v hvc1",
          " -pix_fmt yuv420p10le",
          " -an",
          " ", file.path("docs", "usdm", "latest.mp4"),
          " -y"),
        wait = TRUE
      )
      
      system2(
        command = "ffmpeg",
        args = paste0(
          "-i ", file.path("docs", "usdm", "latest.mp4"),
          " -c:v libvpx-vp9",
          " -crf 30",
          " -b:v 0",
          " -row-mt 1",
          " ", file.path("docs", "usdm", "latest.webm"),
          " -y"),
        wait = TRUE
      )
    }
    
    return(out)
  }
