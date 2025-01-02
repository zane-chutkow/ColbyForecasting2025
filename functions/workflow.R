#' Read an workflow given the version and path
#' 
#' @param version chr the version to read
#' @param path chr the path to the data directory
#' @return workflow
read_workflow = function(version = "g_Aug",
                         path = data_path("models")){
  filename = file.path(path,
                       sprintf("%s_workflow.rds", version))
  if (!file.exists(filename)) stop("file not found:", filename)
  readr::read_rds(filename )
}

#' Write an workflow given the workflow, version and path
#' 
#' @param x workflow object
#' @param version chr the version to write
#' @param path chr the path to the data directory
#' @return workflow itself is returned
write_workflow = function(x,
                          version = "g_Aug",
                          path = data_path("models")){
  filename = file.path(path,
                       sprintf("%s_workflow.rds", version))
  readr::write_rds(model, filename |>
                     butcher::axe_env())
}
