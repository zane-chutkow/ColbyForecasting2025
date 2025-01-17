DB = brickman_database() |>
  print()
brickman_variables("all")

db = DB |> dplyr::filter(scenario == "RCP45", year == "2055",interval == "mon")
static_vars = read_brickman(db)



buoys = gom_buoys()

buoy = filter(buoys,id == "M01")

long_values = extract_brickman(static_vars, buoys)

longvals = filter(long_values,point == "p5", name == "SST")

longvals = longvals |>
  mutate(month = factor(month, levels = month.abb))



ggplot(data = longvals,mapping = aes(x = month, y = value)) +
  geom_point() + 
  labs(title = "Temp Difference at Buoy M01")