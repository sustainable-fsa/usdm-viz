get_oconus <-
  function(layer = "counties",
           year = 2024){
    
    dir.create("docs/census",
               recursive = TRUE,
               showWarnings = FALSE)
    
    outfile <- 
      file.path(
        "docs",
        "census",
        paste0("census-", year, ".topojson")
      )
    
    if (!file.exists(outfile)) {
      
      counties <-
        tigris::counties(cb = TRUE,
                         resolution = "5m",
                         year = year) %>%
        dplyr::arrange(STATEFP, COUNTYFP) %>%
        dplyr::filter(!(STATEFP %in% c("60", "78", "14", "52", "69", "66"))) %>%
        sf::st_transform("WGS84") %>%
        dplyr::transmute(id = paste0(STATEFP, COUNTYFP)) %>%
        dplyr::distinct()
      
      counties <-
        counties %>%
        dplyr::select(id) %>%
        rmapshaper::ms_explode(sys = TRUE,
                               sys_mem = 16) %>%
        rmapshaper::ms_dissolve(field = "id",
                                sys = TRUE,
                                sys_mem = 16) %>%
        # rmapshaper::ms_simplify(keep = 0.5,
        #                         sys = TRUE,
        #                         sys_mem = 16) %>%
        sf::st_make_valid() %>%
        rmapshaper::ms_explode(sys = TRUE,
                               sys_mem = 16) %>%
        rmapshaper::ms_dissolve(field = "id",
                                sys = TRUE,
                                sys_mem = 16) %>%
        rmapshaper::ms_explode(sys = TRUE,
                               sys_mem = 16) %>%
        tigris::shift_geometry() %>%
        sf::st_make_valid() %>%
        sf::st_transform("WGS84") %>%
        rmapshaper::ms_explode(sys = TRUE,
                               sys_mem = 16) %>%
        rmapshaper::ms_dissolve(field = "id",
                                sys = TRUE,
                                sys_mem = 16) %>%
        sf::st_cast("MULTIPOLYGON") %>%
        dplyr::arrange(id) %>%
        dplyr::left_join(
          counties %>%
            sf::st_drop_geometry() %>%
            dplyr::distinct()
        ) %>%
        dplyr::mutate(state = stringr::str_sub(id, 1, 2),
                      county = stringr::str_sub(id, 3, 5)) %T>%
        sf::write_sf(stringr::str_replace(outfile, "topojson", "geojson"),
                     delete_dsn = TRUE)
      
      system(
        paste0(
          "
mapshaper \\
  ", stringr::str_replace(outfile, "topojson", "geojson"), " \\
  -clean rewind overlap-rule=max-id \\
  -rename-layers counties,states,oconus \\
  -dissolve field=state copy-fields='id' + name=states \\
  -each 'id=id.slice(0,2)' target=states \\
  -dissolve + name=oconus \\
  -rename-layers counties,states,oconus \\
  -o format=topojson quantization=1e5 fix-geometry id-field='id' bbox target=* ", outfile, "
"
        )
        
      )
      
      unlink(
        stringr::str_replace(outfile, "topojson", "geojson")
      )
    }
    
    return(
      sf::read_sf(outfile,
                  layer = layer) %>%
        sf::st_set_crs("WGS84") %>%
        sf::st_transform("ESRI:102003")
    )
  }

# get_oconus(layer = "counties")
# get_oconus(layer = "states")
# get_oconus(layer = "oconus")
