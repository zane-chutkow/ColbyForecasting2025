# use fsprint with species, month and approach too
#return the model input. Provide an sf table
#given species, month, and sampling approach give a result. Print as a table

#' Reads a model input file given species, month, approach and path
#' 
#' @param scientificname chr, the species name
#' @param mon chr month abbreviation
#' @param approach chr, one of "greedy" or "conservative"
#' @param path chr the path to the data directory
read_model_input = function(scientificname = "Homarus americanus",
                            mon = "Jan",
                            approach = "greedy",
                            path = data_path("model_input")){
  # your part goes in here
  if(FALSE){
    scientificname = "Homarus americanus"
    mon = "Jan"
    approach = "greedy"
    path = data_path("model_input")
  }
  
  filename = sprintf("%s-%s-%s_input.gpkg", 
                     gsub(" ", "_", scientificname),
                     mon,approach)

  filename = file.path(path,filename)
  x = read_sf(filename)
  
  x
  
  
}