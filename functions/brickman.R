
brickman_variables = function(interval = c("mon", "ann", "all")[1]){
  
  #' Retrieve a listing of all Brickman variables
  #' 
  #' @param interval chr, one of "mon" (monthly), "ann" (annual) or "all"
  #' @return table
  
  x = dplyr::tribble(
    ~name, ~group, ~longname, ~description, ~units,
    'depth', "static", "Bathy_depth", "bathymetric depth", "m",
    'mask',  "static", "land_mask", "land mask", "",
    'SST', "ann", "SST_ann", "annual SST", "C",
    'SST', "mon", "SST", "monthly SST", "C",
    'MLD', "ann", "MLD_ann", "annual mixed layer depth", "m",
    'MLD', "mon", "MLD", "monthly mixed layer depth", "m",
    'SSS', "ann", "SSS_ann", "annual SSS", "psu",
    'SSS', "mon", "SSS", "monthly SSS", "psu",
    'Tbtm', "ann", "Tbtm_ann", "annual Tbtm", "C",
    'Tbtm', "mon", "Tbtm", "monthly Tbtm", "C",
    'Sbtm', "ann", "Sbtm_ann", "annual Sbtm", "psu",
    'Sbtm', "mon", "Sbtm", "monthly Sbtm", "psu",
    'Xbtm', "ann", "Xbtm_ann", "annual Xbtm", "unknown",
    'Xbtm', "mon", "Xbtm", "monthly Xbtm", "unknown",
    'U', "ann", "U_ann", "annual U", "m/s",
    'U', "mon", "U", "monthly U", "m/s",
    'V', "ann", "V_ann", "annual V", "m/s",
    'V', "mon", "V", "monthly V", "m/s")
  
  switch(tolower(interval[1]),
         "mon" = dplyr::filter(x, .data$group %in% c("static", "mon")),
         "ann" = dplyr::filter(x, .data$group %in% c("static", "ann")),
         x)
}





brickman_database = function(path = file.path(ROOT_DATA_PATH, "brickman")){
  
  #' Read the Brickman database
  #' 
  #' @param path chr, the path to the data
  #' @return tabular database
  
  readr::read_csv(file.path(path, "database.csv"), col_types = "cccc")
}


brickman_build_database = function(path = brickman_path()){
  
  #' Build the Brickman database
  #' 
  #' @param path chr, the path to the data
  #' @return tabular database
  
  ff = list.files(path, pattern = glob2rx("*.tif"))
  ff = gsub(".tif", "", ff, fixed = TRUE) |>
    strsplit( "_", fixed = TRUE) 
  dplyr::tibble(
    scenario = sapply(ff, "[[", 1),
    year = sapply(ff, "[[", 2),
    interval = sapply(ff, "[[", 3),
    var = sapply(ff, "[[", 4)) |>
  readr::write_csv(file.path(path, "database.csv"))
}




read_brickman = function(db = brickman_database() |>
                           dplyr::filter(scenario == "PRESENT",
                                         year == "PRESENT",
                                         interval == "mon"), 
                         add_depth = TRUE, 
                         path = brickman_path()){

  #' Read Brickman data given a database
  #' 
  #' Intervals are exclusive - you can't read "mon" and "ann" into the same variable.
  #' If you need two or more groups then read into two or more variables.
  #' 
  #' Years are exclusive - you can't read 2055 and 2075 into the same variable.
  #' If you need two or more years then read into two or more variables.
  #' 
  #' Scenarios are exclusive - you can't read RCP85 and RCP45 into the same variable.
  #' If you need two or more scenarios then read into two or more variables.
  #' 
  #' @param db table of database of one group type
  #' @param add_depth logical, if TRUE include depth as a variable
  #' @param path chr, the brickman data path
  #' @return stars array
  
  if(length(unique(db$interval)) > 1) {
    stop("please just read from one interval at a time: mon, ann or static")
  }
  
  if(length(unique(db$scenario)) > 1) {
    stop("please just read from one scenario at a time: RCP85, RCP45, PRESENT")
  }
  if(length(unique(db$year)) > 1) {
    stop("please just read from one year at a time: 2055, 2075, PRESENT")
  }  
  
  x = dplyr::rowwise(db) |>
      dplyr::group_map( 
             function(tbl, key){
               filename = file.path(path, 
                                    sprintf("%s_%s_%s_%s.tif", 
                                            tbl$scenario, 
                                            as.character(tbl$year), 
                                            tbl$interval,
                                            tbl$var))

               x = if (tbl$interval == "mon"){
                  #dates = switch(as.character(tbl$year),
                  #  "PRESENT" = seq(from = as.Date("2000-01-01"),
                  #                  length = 12, by = "month"),
                  #  "2055" = seq(from = as.Date("2055-01-01"),
                  #               length = 12, by = "month"),
                  #  "2075" = seq(from = as.Date("2075-01-01"),
                  #               length = 12, by = "month"))
                  stars::read_stars(filename) |>
                   stars::st_set_dimensions("band",
                                            values = month.abb,
                                            names = "month")
                   
                 } else {
                   stars::read_stars(filename)
                 } 
             rlang::set_names(x, tbl$var)
             }) 
  x = do.call(c, append(x, list(along = NA_integer_)))
  
  if (add_depth && !("depth" %in% names(x))){
    db = brickman_database()
    depth = read_brickman(db |> filter(scenario == "STATIC", var == "depth"))
    if (length(dim(x) == 3)) {
      dd = sapply(month.abb, function(mon) {depth}, simplify = FALSE)
      depth = do.call(c, append(dd, list(along = list(month = month.abb))))
    }
    x = c(x, depth)
  } # add depth?
  
  return(x)
}


extract_brickman = function(x = read_brickman(), 
                            y = gom_buoys(), 
                            form = "long"){
  
  #' Extract point data from a Brickman stars object
  #' 
  #' @param x stars object (with or without month dimension)
  #' @param y sf point data
  #' @param form chr one of either "long" (the default) or "wide" to control 
  #'   output column layout. 
  #' @param ... other arguments passed to `stars::st_extract()`
  #' @return table of variable data for each input point
  
  
  n = nrow(y)
  N = floor(log10(n)+ 1)
  fmt = paste0("p%0.", N, "i")
  pnames = sprintf(fmt, seq_len(n))
  xy = sf::st_coordinates(y)
  if (length(dim(x)) == 2){
  
    m = stars::st_extract(x, xy)
    rownames(m) <- pnames
    p = dplyr::as_tibble(m, rownames = "point") |>
      tidyr::pivot_longer(-dplyr::all_of("point"),
                          values_to = "value",
                          names_to = "name")
     
  } else {
    nms = stars::st_get_dimension_values(x, 3)
    p = lapply(names(x),
     function(nm){
       m = stars::st_extract(x[nm], xy)
       colnames(m) <- nms
       rownames(m) <- pnames
       m |>
         dplyr::as_tibble(rownames = "point") |>
         dplyr::mutate(name = nm, .after = 1)
     }) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(-dplyr::all_of(c("point", "name")),
                              names_to = "month",
                              values_to = "value")
  }
  if (tolower(form[1]) == "wide"){
    p = tidyr::pivot_wider(p,
                           names_from = "name",
                           values_from = "value")
  }
  
  p
}


assemble_brickman= function(scenario = c("RCP85", "RCP45", "PRESENT"),
                           year = c("2055", "2075", "PRESENT"),
                           vars = "all", 
                           interval = c("mon", "ann"), 
                           bb = colby_bbox(),
                           band_as_time = FALSE,
                           path = brickman_path()){
  
  #' Read, warp, subset and archive all combinations of the raw Brickman data
  #' 
  #' 
  #' @param scenario chr, one o rmore of "RCP85", "RCP45", or "PRESENT"
  #' @param year chr, year "2055" or "2075"
  #' @param vars chr, variable names (such as "SST" or "all")
  #' @param interval chr, either "ann" or "mon" (default)
  #' @param bb sf, something which provides a bounding box for subsetting
  #' @param band_as_time logical, convert band to appropriate time/date stamp
  #' @param path filepath, path to where you want data saved
  #' @return tibble database
  
  
  if(FALSE){
    scenario = c("RCP85", "RCP45", "PRESENT")
    year = c("2055", "2075", "PRESENT")
    vars = "all"
    interval = c("mon", "ann")[1]
    bb = colby_bbox()
    band_as_time = FALSE
    path = brickman_path()
  }
  
  # first load PRESENT
  # for each interval (mon and ann) of each scenario (RCP85 and (RCP45) of each year (2055 and 2075) 
  #  load (PRESENT + departure)
  
  if(!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  
  # deal with bathymetry and land mask separately
  
  for (int in c("mon", "ann")){
    vars = brickman_layers(interval = int)
    present = brickman::read_brickman(scenario = "PRESENT", 
                                      year = NA, 
                                      vars = vars |> 
                                        dplyr::filter(group != "static") |>
                                        dplyr::pull(name), 
                                      interval = int) |>
      brickman::warp_brickman(crs = sf::st_crs(bb))
    present = present[bb]
    for (nm in names(present)){
      filename = file.path(path, sprintf("%s_%s_%s_%s.tif", "PRESENT", "PRESENT", int, nm))
      ok = present[nm,,, drop = FALSE] |>
        stars::write_stars(filename)
    } # name of var
  
    for (scenario in c("RCP85", "RCP45")) {
      for (year in c("2055", "2075")){
        s = brickman::read_brickman(scenario = scenario, 
                                year = year, 
                                vars = vars |> 
                                  dplyr::filter(group != "static") |>
                                  dplyr::pull(name), 
                                interval = int) |>
          brickman::warp_brickman(crs = sf::st_crs(bb))
        s = s[bb]
        for (nm in names(s)) {
          if (nm %in% names(present)) s[[nm]] = s[[nm]] + present[[nm]]
        } # name of var
        for (nm in names(s)){
          filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario, year, int, nm))
          ok = s[nm,,, drop = FALSE] |>
            stars::write_stars(filename)
        } # name of var
      } #year
    } # scenario
  } # int
  
  bathy = brickman::read_brickman(scenario = "PRESENT", year = NA, var = "Bathy_depth") |>
    brickman::warp_brickman(crs = sf::st_crs(bb))
  bathy = bathy[bb] |>
    stars::write_stars(file.path(path, "STATIC_STATIC_static_depth.tif"))
  mask = brickman::read_brickman(scenario = "PRESENT", year = NA, var = "land_mask") |>
    brickman::warp_brickman(crs = sf::st_crs(bb))
  mask = mask[bb] |>
    stars::write_stars(file.path(path, "STATIC_STATIC_static_mask.tif"))
  
}