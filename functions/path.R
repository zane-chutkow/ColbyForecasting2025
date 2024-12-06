#' A function to retrieve a dates year as a 4-digit character string
#' 
#' @param date Date or POSIXct class object
#' @return the year as 4 character string
get_year = function(date = Sys.Date()){
  format(date, "%Y")
}


#' A function to retrieve the username
#' 
#' @return username
get_user = function(){
  system("whoami", intern = TRUE)
}

#' Make a path if it doesn't exist
#' 
#' @param path chr the path to create
#' @return the path
make_path = function(path){
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}


#' Get the personal  data path
#' 
#' @param user chr, the user's name (no spaces please)
#' @param year chr, the current year as 4-character string
#' @param root chr, the root directory for data
#' @return chr personal data path
personal_data_path = function(user = get_user(),
                              year = get_year(),
                              root = ROOT_DATA_PATH){
  path = file.path(root, year, user)
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}

#' Get the data path
#' 
#' @param what chr path and file segments to append to the root path using the
#'   system file separator
#' @param root chr, the root personal data directory
#' @return some directory in the personal data path
data_path = function(..., root = personal_data_path()){
  file.path(root, ...)
}

#' Retrieve the path to the brickman data
#' 
#' @param root chr, the root global data directory
#' @return the path to the brickman data directory
brickman_path = function(root = ROOT_DATA_PATH){
  file.path(root, "brickman")
}


#' Retrieve the path to the coastline data
#' 
#' @param root chr, the root global data directory
#' @return the path to the coastline data directory
coastline_path = function(root = ROOT_DATA_PATH){
  file.path(root, "coastline")
}


#' Retrieve the path to the obis data
#' 
#' @param root chr, the root personal data directory
#' @return the path to the brickman config file
obis_path = function(root = personal_data_path()){
  path = file.path(root, "obis")
  if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  return(path)
}