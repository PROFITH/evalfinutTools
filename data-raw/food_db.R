library(readr)
library(dplyr)
library(janitor)

# 1. Read the clean CSV
# read_csv automatically handles dot decimals and comma separators
food_db = data.table::fread("inst/extdata/food_composition_data.csv", data.table = F)

# 3. Save to package
usethis::use_data(food_db, overwrite = TRUE)
