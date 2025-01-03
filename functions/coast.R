
read_coastline = function(what = "medium",
                          path = coastline_path()){
  
  #' Read a coastline geometry set
  #' 
  #' @param what chr one of "coarse", "medium" or "detail"
  #' @param path chr the data path o the coastline products
  #' @return sfc coastline object
  
  filename = file.path(path, paste0("coastline_", tolower(what[1]), ".gpkg"))
  if (!file.exists(filename)) stop("file not found:", filename)
  sf::read_sf(filename)
}



prep_coastlines = function(output_path = coastline_path(),
                           bb = colby_bbox()){
  
  #' Prepare the to "coarse", "medium" and "detail" coastlines
  #' 
  #' @param output_path chr the path where we save the data
  #' @param bb bounding box for clipping
  #' @return chr the names of the three saved files
  
  if (!dir.exists(output_path)) ok = dir.create(output_path, recursive = TRUE)
  what = c("coarse" = 110, "medium" = 50, "detail" = 10)
  sapply(names(what),
         function(wh){
           filename = file.path(output_path, paste0("coastline_", wh, ".gpkg"))
           coast = rnaturalearth::ne_coastline(scale = what[[wh]], returnclass = "sf") |>
             sf::st_geometry() |>
             sf::st_crop(bb) |>
             sf::st_transform(crs = 4326) |>
             sf::write_sf(filename)
           return(filename)
         })
}