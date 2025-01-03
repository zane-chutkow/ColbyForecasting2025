
read_configuration = function(version = "v001",
                              path = data_path("models")){
  
  #' Given a version identifier, read a configuration
  #' 
  #' @param version chr the version identifier (defaults to 'v001')
  #' @param path chr you personal model data path (defaults to `data_path("models")`)
  #' @return configuration list
  
  filename = file.path(path, paste0(version[1], ".yaml"))
  yaml::read_yaml(filename)
}



write_configuration = function(cfg = list(version = "v000", species = "Canis familiaris"),
                              path = data_path("models")){
  
  #' Given a versioned configuration write a configuration
  #' 
  #' @param cfg list, must have element named "version"
  #' @param path chr you personal model data path (defaults to `data_path("models")`)
  #' @return the input configuration list
  
  if (!"version" %in% names(cfg)) stop("input config must have version element")
  
  filename = file.path(path, paste0(cfg$version[1], ".yaml"))
  yaml::write_yaml(cfg, filename)
}



read_configurations = function(versions,
                               path = data_path("models")){
  
  #' Read one or more configurations into a table. 
  #' 
  #' Note that if configurations differ in the number of elements, the output
  #' will contains ALL elements (the union of elements) with missing items assigned
  #' to NA or NULL as appropriate.
  #' 
  #' @param versions chr one or more versions
  #' @param path the model data path
  #' @return table of config values
  
  x = lapply(versions,
             function(ver){
               x = read_configuration(ver, path = path) |>
                 dplyr::as_tibble()
               return(x)
             }) |>
    dplyr::bind_rows()
  return(x)
}