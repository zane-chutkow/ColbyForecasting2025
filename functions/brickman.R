#' Set the brickman path
#' 
#' @param path chr, the path to the brickman data
#' @param filename chr, the name of the file to store the path
#' @param the known brickman path
set_brickman_path = function(path = "/mnt/s1/projects/ecocast/projects/ColbyForecasting/data/brickman", 
                             filename = "~/.colby-brickman"){
  cat(path, sep = "\n", file = filename)
  brickman_path()

}
#' Retrieve the path to the brickman data
#' 
#' @param filename the name of the config file where brickman data is stored
#' @return the path to the brickman config file
brickman_path = function(filename = "~/.colby-brickman"){
  if(!file.exists(filename)) stop("please use set_brickman_path() first")
  readLines(filename)
}

#' Retrieve a listing of all Brickman variables
#' 
#' @param interval chr, one of "mon" (monthly), "ann" (annual) or "all"
#' @return table
brickman_layers = function(interval = c("mon", "ann", "all")[1]){
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


#' Read the Brickman database
#' 
#' @param path chr, the path to the data
#' @return tabular database
brickman_database = function(path = brickman_path()){
  readr::read_csv(file.path(path, "database.csv"), col_types = "cccc")
}

#' Build the Brickman database
#' 
#' @param path chr, the path to the data
#' @return tabular database
brickman_build_database = function(path = brickman_path()){
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
#' @param path chr, the brickman data path
#' @return stars array
read_brickman = function(db = brickman_database() |>
                           dplyr::filter(scenario == "PRESENT",
                                         year == "PRESENT",
                                         interval == "mon"), 
                         path = brickman_path()){

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
                  dates = switch(as.character(tbl$year),
                    "PRESENT" = seq(from = as.Date("2000-01-01"),
                                    length = 12, by = "month"),
                    "2055" = seq(from = as.Date("2055-01-01"),
                                 length = 12, by = "month"),
                    "2075" = seq(from = as.Date("2075-01-01"),
                                 length = 12, by = "month"))
                  stars::read_stars(filename) |>
                   stars::st_set_dimensions("band",
                                            values = dates,
                                            names = "date")
                   
                 } else {
                   stars::read_stars(filename)
                 } 
             rlang::set_names(x, tbl$var)
             }) 
  do.call(c, append(x, list(along = NA_integer_)))
}



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
assemble_brickman= function(scenario = c("RCP85", "RCP45", "PRESENT"),
                           year = c("2055", "2075", "PRESENT"),
                           vars = "all", 
                           interval = c("mon", "ann"), 
                           bb = colby_bbox(),
                           band_as_time = FALSE,
                           path = brickman_path()){
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