DB = brickman_database() |>
  print()
brickman_variables("all")

db = DB |> dplyr::filter(interval == "static")
static_vars = read_brickman(db)



buoys = gom_buoys()

buoy = filter(buoys,id == "M01")

long_values = extract_brickman(x, buoys)

longvals = filter(long_values,point == "p5")

short = select(long_values,month,value)

trial = filter(long_values, point == "p5")

longvals
