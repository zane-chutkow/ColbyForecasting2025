
predict_table = function(wflow, newdata, type = "prob", threshold = 0.5, ...){
  
  #' Predict a classification from a table of new data
  #' 
  #' @param wflow a workflow object
  #' @param newdata table of new data including a `class` variable
  #' @param typ chr the type of prediction, by default "prob" - see `?predict.model_fit`
  #' @param threshold num the aribitray threshold used to define the outcome
  #' @param ... other arguments for `predict.model_fit`
  #' @return a table of predictions with the orginal class label
  
  lvls = levels(newdata$class)
  predict(wflow, newdata, type = type, ...) |>
    dplyr::mutate(.pred = if_else(.pred_presence >= threshold, lvls[1], lvls[2]) |>
                    factor(levels = lvls),
                  class = newdata$class)
}


predict_stars = function(wflow, newdata, type = "prob", threshold = 0.5, ...){
  
  #' Predict a classification from stars object
  #' 
  #' @param wflow a workflow object
  #' @param newdata stars data
  #' @param typ chr the type of prediction, by default "prob" - see `?predict.model_fit`
  #' @param threshold num the aribitray threshold used to define the outcome
  #' @param ... other arguments for `predict.model_fit`
  #' @return a stars object
  lvls = c("presence", "background")
  predict(newdata, wflow, type = type, ...) |>
    dplyr::mutate(.pred = if_else(.pred_presence >= threshold, lvls[1], lvls[2]) |>
                    factor(levels = lvls))
}