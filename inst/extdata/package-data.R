library(devtools)
load_all()
taxa_clean <- read.csv("./inst/extdata/clean.csv")
taxa_remove <- read.csv("./inst/extdata/remove.csv")
taxa_combine <- read.csv("./inst/extdata/combine.csv")
usethis::use_data(
  taxa_clean, 
  taxa_remove, 
  taxa_combine
)