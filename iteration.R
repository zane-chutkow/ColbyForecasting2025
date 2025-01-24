for (month in month.abb){
  iterMon = month
  model_input = read_model_input(scientificname = "Homarus americanus", 
                                 approach = "greedy", 
                                 mon = iterMon)
  
  
  versionname = sprintf("g_%s", gsub(" ", "_", iterMon))
  cfg = read_configuration(version = versionname)
  db = brickman_database()
  covars = read_brickman(db |> filter(scenario == "PRESENT", interval == "mon"))|>
    select(all_of(cfg$keep_vars))
  
  all_data = prep_model_data(model_input, 
                             month = iterMon,
                             covars = covars, 
                             form = "table") |>
    select(all_of(c("class", cfg$keep_vars)))
  
  # A little function to compute the ratio of presences to background
# @param x table with a "class" column
# @return numeric ratio presences/background
get_ratio = function(x){
  counts = count(x, class)
  np = filter(counts, class == "presence") |> pull(n)
  nb = filter(counts, class == "background") |> pull(n)
  return(np/nb)
}

cat("ratio of presence/background in full data set:", get_ratio(all_data), "\n")

split_data = initial_split(all_data, 
                           prop = 3/4,
                           strata = class)

wflow = workflow()

tr_data = training(split_data)


rec = recipe(class ~ ., data = slice(tr_data,1))

rec = rec |> 
  step_naomit()
if ("depth" %in% cfg$keep_vars){
  rec = rec |>
    step_log(depth,  base = 10)
}
if ("Xbtm" %in% cfg$keep_vars){
  rec = rec |>
    step_log(Xbtm,  base = 10)
}

wflow = wflow |>
  add_recipe(rec)

model = rand_forest() |>
  set_mode("classification") |>
  set_engine("ranger", probability = TRUE, importance = "permutation") 

wflow = wflow |>
  add_model(model)

fitted_wflow = fit(wflow, data = tr_data)

train_pred = predict_table(fitted_wflow, tr_data, type = "prob")

write_workflow(fitted_wflow, version = cfg$version)

}