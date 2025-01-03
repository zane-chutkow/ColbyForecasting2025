
plot_roc = function(x, truth, pred, title = "ROC"){
  
  #' Plot an annotated ROC with AUC
  #' 
  #' @param x table of predictive outcomes - see `predict_model`
  #' @param truth the truth column, usually `class`
  #' @param pred the prediction column, usually .pred_presense
  #' @param title chr the optional title
  #' @return ggplot2 object suitable for printing
  
  auc = yardstick::roc_auc(x, {{truth}}, {{pred}}) |>
    dplyr::pull(.estimate)
  roc_curve(x, {{truth}}, {{pred}}) |>
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_bw() + 
    labs(x = "False Positive Rate (Specificity)",
         y = "True Positive Rate (Sensitivity)",
         title = title) + 
    ggplot2::annotate("text", x = 0.8, y= 0.05, 
                      label = sprintf("AUC: %0.3f", auc)) 
}
