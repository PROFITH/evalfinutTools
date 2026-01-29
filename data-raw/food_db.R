library(readr)
library(dplyr)
library(janitor)

food_db_add = data.table::fread("inst/extdata/evalfinut-export-20260129141328.xls")

remove = which(!food_db_add$name %in% c("Seaweed, wakame, raw", "Ravioli, cheese-filled, canned") &
                 food_db_add$origin == "USDA")
food_db_add = food_db_add[-remove,]

# 1. Read the clean CSV
# read_csv automatically handles dot decimals and comma separators
food_db = data.table::fread("inst/extdata/food_composition_data.csv", data.table = F)

food_db_merged = merge(food_db[, c("id", "NOVA")],
                       food_db_add, by = "id", all = T)


write.csv(food_db_add, "inst/extdata/food_composition_data_ext.csv", row.names = F, na = "")
# 3. Save to package
usethis::use_data(food_db, overwrite = TRUE)
