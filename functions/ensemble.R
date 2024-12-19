#' Read an ensemble given the version and path
#' 
#' @param version chr the version to read
#' @param path chr the path to the data directory
#' @return ensemble
read_ensemble = function(version = "g008",
                         path = data_path("models")){
  filename = file.path(path,
                       sprintf("%s_ensemble.rds", version))
  if (!file.exists(filename)) stop("file not found:", filename)
  readr::read_rds(filename)
}

#' Write an ensemble given the ensemble, version and path
#' 
#' @param ensemble a SDM ensemble
#' @param version chr the version to write
#' @param path chr the path to the data directory
#' @return ensemble itself
write_ensemble = function(ensemble,
                        version = "g008",
                         path = data_path("models")){
  filename = file.path(path,
                       sprintf("%s_ensemble.rds", version))
  readr::write_rds(ensemble, filename)
}

