library(readr)
library(dplyr)
library(janitor)

food_db = data.table::fread("inst/extdata/food_composition_data.csv",
                                data.table = F)

# 3. Save to package
usethis::use_data(food_db, overwrite = TRUE)
