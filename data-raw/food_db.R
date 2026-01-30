library(readr)
library(dplyr)
library(janitor)

food_db_add = data.table::fread("inst/extdata/food_composition_data.csv")

# 3. Save to package
usethis::use_data(food_db, overwrite = TRUE)
