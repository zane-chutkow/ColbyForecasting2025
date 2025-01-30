
read_observations = function(scientificname = "Doryteuthis pealeii",
                             minimum_year = 1974, maximum_year = 2070, basis = NULL, 
                             minCount = NULL,maxCount = NULL, filtNAIndCount = FALSE
                             ){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param basis, an optional argument to be used to select only a certain basis of record
  #' @param minCount and maxCount, optional parameters to filter a range of individual counts to be included
  #' @param other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = read_obis(scientificname)
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  if (!is.null(maximum_year)){
    x = x |>
      filter(year <= maximum_year)
  }
  
  # if the user provided a non-Null filter by observation type
  if(!is.null(basis)){
    x = x |> filter(basisOfRecord == basis)
  }
  #default basisOfObservation filtering
  else{
    x = x |> filter(basisOfRecord == "HumanObservation" |
                      basisOfRecord == "Occurrence")
  }
  
  x = x |> filter(!is.na(eventDate))
  
  if ((!is.null(minCount)) && (!is.null(maxCount)) && (minCount < maxCount)) {
    x = x|> filter(individualCount > minCount, individualCount < maxCount)
  }
  
  if (filtNAIndCount == TRUE){
  x = x |> filter(!is.na(individualCount))
  }
  
  db = brickman_database() |>
    filter(scenario == "STATIC", var == "mask")
  mask = read_brickman(db, add_depth = FALSE)
  hitOrMiss = extract_brickman(mask, x)
  
  
  
  return(x)
}

  
