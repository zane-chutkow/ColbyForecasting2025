# Note: this is a convenience function to update the ColbyForecastingDacs package
# which includes pushing onto github at https://github.com/BigelowLab/ColbyForecastingDocs
#
# It only makes sense to use if you have push rights to that repos
update_help_docs = function(path = "~/Library/CloudStorage/Dropbox/code/projects/ColbyForecastingDocs"){
  source(file.path(path, "inst/update_script.R"))
}


install_docs = function(repos = "BigelowLab/ColbyForecastingDocs", ...){
  #' Install the Colby Forecasting Documentation package
  #' 
  #' Please see \href{https://github.com/BigelowLab/ColbyForecastingDocs}{ColbyForecastingDocs}
  #' @param repos chr, the namespace and name of the package on github
  #' @param ... other arguments for \code{\link[remotes]{install_github}}
  #' @return see \code{\link[remotes]{install_github}}
  
  remotes::install_github(repos, ...)
}