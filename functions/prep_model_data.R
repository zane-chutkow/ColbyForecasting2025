
prep_model_data = function(model_input,
                           month = "Jan",
                           covars = NULL,
                           depth = NULL,
                           keep_vars = c(names(covars), "depth"),
                           form = c("sf", "table")[1]){
  
  #' Given a selection of POINT model_input data, prepare the table needed to 
  #' model
  #' 
  #' @param model_input `sf` with at least one attribute ("class").
  #' @param month the name of the month
  #' @param covars NULL or stars object (with 12 months)
  #' @param depth NULL or stars object with depth
  #' @param keep_vars chr, a vector of variable to keep, if NULL keep all
  #' @param form chr one of "sf" or "table" (to drop the spatial info)
  #' @return a plain table or `sf` table with attributes (variables) added
  
  if (is.null(covars) || is.null(depth)){
    db = brickman_database()
    if (is.null(depth)) depth = read_brickman(db |> 
                                                dplyr::filter(scenario == "STATIC",
                                                              var == "depth"))
    if (is.null(covars)) covars = read_brickman(db |> 
                                                  dplyr::filter(scenario = "PRESENT",
                                                                interval = "mon"))
  }
 
  month_covars = covars |> 
    dplyr::slice("month", month) |>
    bind_attrs(depth)
  
  variables = model_input |>
    dplyr::bind_cols(extract_brickman(month_covars, model_input, form = "wide")) |>
    dplyr::select(-point) |> 
    dplyr::select(dplyr::all_of(c("class", keep_vars))) |>
    dplyr::mutate(class = factor(class, levels = c("presence", "background"))) |>
    tidyr::drop_na()
  
  if (tolower(form[1]) == "table") variables = sf::st_drop_geometry(variables)
  variables
}
