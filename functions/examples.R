
read_sunspots = function(){
  
  #' Read in a sunspot data
  #' 
  #' @return table of monthly sunspot activity
  #' @examples
  #' read_sunspots() |>
  #'  group_by(month) |>
  #'  group_modify(
  #'   function(tbl, key){
  #'     dplyr::tibble(
  #'     month = key$month,
  #'     min = min(tbl$sunspots),
  #'     q1 = quantile(tbl$sunspots, 0.25),
  #'     mean = mean(tbl$sunspots),
  #'     median = mean(tbl$sunspots),
  #'     q3 = quantile(tbl$sunspots, 0.75),
  #'     max = max(tbl$sunspots))
  #'   })
  
  s = start(sunspot.month)
  s = sprintf("%0.4i-%0.2i-01", s[1], s[2]) |>
    as.Date()
  e = end(sunspot.month)
  e = sprintf("%0.4i-%0.2i-01", e[1], e[2]) |>
    as.Date()
  date = seq(from = s, to = e, by = "month")
  
  dplyr::tibble(date = date,
                month = format(date, "%b"),
                month_factor = factor(month, levels = month.abb),
                sunspots = as.vector(sunspot.month))
}



read_iris = function(){
  
  #' Read the Anderson iris data set
  #' 
  #' @return table of the famous iris data set
  
  iris |>
    dplyr::as_tibble()
}
