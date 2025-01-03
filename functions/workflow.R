
read_workflow = function(version = "g_Aug",
                         path = data_path("models")){
  #' Read a workflow given the version and path
  #' 
  #' @param version chr the version to read
  #' @param path chr the path to the data directory
  #' @return workflow
  
  filename = file.path(path,
                       sprintf("%s_workflow.rds", version))
  if (!file.exists(filename)) stop("file not found:", filename)
  readr::read_rds(filename) |>
    bundle::unbundle()
}


write_workflow = function(x,
                          version = "g_Aug",
                          path = data_path("models")){
  
  #' Write a workflow given the workflow, version and path
  #' 
  #' @param x workflow object
  #' @param version chr the version to write
  #' @param path chr the path to the data directory
  #' @return workflow itself is returned

  filename = file.path(path,
                       sprintf("%s_workflow.rds", version))
  x |>
    bundle::bundle() |>
    readr::write_rds(filename)
}
