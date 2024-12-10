#' This file is used to check for needed R packages (from CRAN and github). If the 
#' packages are installed locally then they are simply loaded. If they are 
#' not installed then they are installed first and then loaded.


# Here we list the packages by name, CRAN is easier than GITHUB
packages = list(
  CRAN = c("remotes", "dplyr", "ggplot2", "readr", "tidyr", "tidymodels", "sf", 
           "stars", "rnaturalearth", "robis"),
  GITHUB = NULL, #list(tidysdm = c(repo = "EvolEcolGroup/tidysdm", ref = "dev"))
  RUNIV = list(tidysdm = c("https://evolecolgroup.r-universe.dev", 
                             "https://cloud.r-project.org"))
)

# And here we check for prior installations and install locally as needed
installed = installed.packages() |> rownames()
if ("CRAN" %in% names(packages)){
  ix = packages$CRAN %in% installed
  for (package in packages$CRAN[!ix]) {
    install.packages(package)
  }
}

if ("GITHUB" %in% names(packages)){
ix = names(packages$GITHUB) %in% installed
  for (package in names(packages$GITHUB)[!ix]) {
    remotes::install_github(packages$GITHUB[[package]]$repo,
                            ref = packages$GITHUB[[package]]$ref)
  }
}

if ("RUNIV" %in% names(packages)){
  ix = packages$CRAN %in% installed
  for (package in names(packages$RUNIV[!ix])){
    install.packages(package, repos = packages$RUNIV[[package]])
  }
}


# Here we simply load each package form the library of packages
suppressPackageStartupMessages({
  for (package in packages$CRAN) library(package, character.only = TRUE)
  for (package in names(packages$GITHUB)) library(package, character.only = TRUE)
  for (package in names(packages$RUNIV)) library(package, character.only = TRUE, quietly = TRUE)
})

# Next we check the 'functions' directory for ".R" files and source those
for (f in list.files("functions", pattern = glob2rx("*.R"), full.names = TRUE)) {
  source(f, echo = FALSE)
}

# Finally set path to the data m- hopefully as a sibling to the project directory
# The data directory has top level subdirectories ("buoys", "coast", "brickman")
# that contain data used by all.  It also may have one or more yearly directories
# for "personal data directories" ala "2024", "2025"
ROOT_DATA_PATH = "~/Dropbox/code/projects/ColbyForecasting_data/"
