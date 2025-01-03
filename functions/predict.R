
predict_model = function(model, newdata, type = "prob", threshold = 0.5, ...){
  
  #' Predict a classification model
  #' 
  #' @param model a model object
  #' @param newdata table of new data including a `class` variable
  #' @param typ chr the type of prediction, by default "prob" - see `?predict.model_fit`
  #' @param threshold num the aribitray threshold used to define the outcome
  #' @param ... other arguments for `predict.model_fit`
  #' @return a table of predictions with the orginal class label
  
  lvls = levels(newdata$class)
  predict(model, newdata, type = type, ...) |>
    dplyr::mutate(.pred = if_else(.pred_presence >= threshold, lvls[1], lvls[2]) |>
                    factor(levels = lvls),
                  class = newdata$class)
}