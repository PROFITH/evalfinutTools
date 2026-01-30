library(readr)
library(dplyr)
library(janitor)

food_db_add = data.table::fread("inst/extdata/evalfinut-export-20260129141328.xls",
                                data.table = F)
food_db = merge(food_db[, c("id", "NOVA")], food_db_add, by = "id", all = T)

# 3. Save to package
usethis::use_data(food_db, overwrite = TRUE)
