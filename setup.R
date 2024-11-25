#' This file is used to check for needed R packages (from CRAN and github). If the 
#' packages are installed locally then they are simply loaded. If they are 
#' not installed then they are installed first and then loaded.


# Here we list the packages by name, CRAN is easier than GITHUB
packages = list(
  CRAN = c("remotes", "dplyr", "ggplot2", "readr", "tidyr", "tidymodels", "sf", "stars", 
           "rnaturalearth", "robis"),
  GITHUB = list(tidysdm = c(repo = "EvolEcolGroup/tidysdm", ref = "dev"))
)

# And here we check for prior installations and install locally as needed
installed = installed.packages() |> rownames()
ix = packages$CRAN %in% installed
for (package in packages$CRAN[!ix]) {
  install.packages(package)
}
ix = names(packages$GITHUB) %in% installed
for (package in names(packages$GITHUB)[!ix]) {
  remotes::install_github(packages$GITHUB[[package]]$repo,
                          ref = packages$GITHUB[[package]]$ref)
}

# Here we simply load each package form the library of packages
suppressPackageStartupMessages({
  for (package in packages$CRAN) library(package, character.only = TRUE)
  for (package in names(packages$GITHUB)) library(package, character.only = TRUE)
})

# Next we check the 'functions' directory for ".R" files and source those
for (f in list.files("functions", pattern = glob2rx("*.R"))) {
  source(f, echo = FALSE)
}
