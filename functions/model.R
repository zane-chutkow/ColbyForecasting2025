#' Read an model given the version and path
#' 
#' @param version chr the version to read
#' @param path chr the path to the data directory
#' @return model
read_model = function(version = "g_Aug",
                      path = data_path("models")){
  filename = file.path(path,
                       sprintf("%s_model.rds", version))
  if (!file.exists(filename)) stop("file not found:", filename)
  readr::read_rds(filename)
}

#' Write an model given the model, version and path
#' 
#' @param model model object
#' @param version chr the version to write
#' @param path chr the path to the data directory
#' @return model itself
write_model = function(model,
                       version = "g_Aug",
                       path = data_path("models")){
  filename = file.path(path,
                       sprintf("%s_model.rds", version))
  readr::write_rds(model, filename)
}
