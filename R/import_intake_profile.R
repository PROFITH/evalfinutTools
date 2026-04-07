#' Import Dietary Profile with Percentage Adequacy
#'
#' @description
#' Parses EvalFinut summary profiles. Calculates intake, recommendation ranges, 
#' and percentage of adequacy (Intake/Rec * 100).
#'
#' @param file Path to the summary .xlsx or .xls file.
#' @export
import_intake_profile <- function(file) {
  
  # 1. Metadata (Row 1)
  header <- readxl::read_excel(file, n_max = 1, col_names = TRUE)
  H <- data.frame(
    code = header[[1]], 
    age = header[[5]], 
    sex = header[[4]],
    activity_cal = header[[6]],
    stringsAsFactors = FALSE
  )
  
  # 2. Nutrient Table (Skip metadata rows)
  raw_profile <- readxl::read_excel(file, skip = 3)
  colnames(raw_profile)[1:3] <- c("component_es", "intake_str", "rec_str")
  
  # --- INTERNAL HELPER: Translation (Harmonized with Recall Pipeline) ---
  translate_nutrient <- function(es_name) {
    es_name <- gsub("\u03bc", "u", es_name) # Clean mu symbol globally
    base_name <-dplyr::case_when(
      grepl("Energía", es_name) ~ "energy",
      grepl("Grasa total", es_name) ~ "fat_total",
      grepl("saturados", es_name) ~ "fat_saturated",
      grepl("monoinsaturados", es_name) ~ "fat_mono",
      grepl("poliinsaturados", es_name) ~ "fat_poly",
      grepl("Proteína", es_name) ~ "protein",
      grepl("Carbohidratos", es_name) ~ "carbohydrates",
      grepl("Azúcar", es_name) ~ "sugar",
      grepl("Fibra", es_name) ~ "fiber",
      grepl("Agua", es_name) ~ "water",
      grepl("Alcohol", es_name) ~ "alcohol",
      grepl("Hierro", es_name) ~ "iron",
      grepl("Calcio", es_name) ~ "calcium",
      grepl("Potasio", es_name) ~ "potassium",
      grepl("Magnesio", es_name) ~ "magnesium",
      grepl("Sodio", es_name) ~ "sodium",
      grepl("Fósforo", es_name) ~ "phosphorus",
      grepl("Yoduro", es_name) ~ "iodide",
      grepl("Selenio", es_name) ~ "selenium",
      grepl("Zinc", es_name) ~ "zinc",
      grepl("Colesterol", es_name) ~ "cholesterol",
      grepl("Folato", es_name) ~ "folate",
      grepl("Biotina", es_name) ~ "biotin",
      grepl("Tiamina", es_name) ~ "thiamin",
      grepl("Riboflavina", es_name) ~ "riboflavin",
      grepl("niacina", es_name) ~ "niacin",
      grepl("B5", es_name) ~ "vitamin_b5",
      grepl("B6", es_name) ~ "vitamin_b6",
      grepl("B12", es_name) ~ "vitamin_b12",
      grepl("Ácido graso", es_name) ~ gsub("Ácido graso ", "fatty_acid_", es_name),
      grepl("Vitamina", es_name) ~ gsub("Vitamina ", "vitamin_", es_name),
      TRUE ~ es_name
    )
    
    # Append Units based on standard mapping
    case_when(
      base_name == "energy" ~ "energy_kcal",
      base_name %in% c("vitamin_a", "vitamin_d", "folate", "biotin", "vitamin_b12", "iodide", "selenium") ~ paste0(base_name, "_ug"),
      base_name %in% c("iron", "calcium", "potassium", "magnesium", "sodium", "phosphorus", "zinc", 
                       "cholesterol", "vitamin_e", "vitamin_c", "niacin", "vitamin_b5", "riboflavin", "thiamin", "vitamin_b6") ~ paste0(base_name, "_mg"),
      TRUE ~ paste0(base_name, "_g")
    )
  }
  
  # --- INTERNAL HELPER: Range Parsing ---
  parse_ranges <- function(ing_str, rec_str) {
    if (is.na(rec_str) || rec_str == "" || rec_str == "NaN") {
      ing_nums <- as.numeric(stringr::str_extract_all(ing_str, "[0-9.]+")[[1]])
      return(list(val = ing_nums[1], rec_low = NA, rec_high = NA))
    }
    
    rec_nums <- as.numeric(stringr::str_extract_all(rec_str, "[0-9.]+")[[1]])
    if (stringr::str_detect(rec_str, "-") && length(rec_nums) >= 2) {
      rec_low <- rec_nums[1]; rec_high <- rec_nums[2]
    } else {
      rec_low <- rec_high <- rec_nums[1]
    }
    
    target_in_kcal <- stringr::str_detect(rec_str, "kcal")
    ing_nums <- as.numeric(stringr::str_extract_all(ing_str, "[0-9.]+")[[1]])
    val_intake <- if(target_in_kcal && length(ing_nums) >= 2) ing_nums[2] else ing_nums[1]
    
    return(list(val = val_intake, rec_low = rec_low, rec_high = rec_high))
  }
  
  # 3. Processing
  profile_clean <- raw_profile %>%
    dplyr::filter(!is.na(intake_str)) %>%
    {
      data <- .
      purrr::map2_dfr(data$intake_str, data$rec_str, ~ as.data.frame(parse_ranges(.x, .y))) %>%
        dplyr::bind_cols(data, .)
    } %>%
    dplyr::mutate(
      name_en = translate_nutrient(component_es),
      name_en = janitor::make_clean_names(name_en, 
                                          replace = c("\u03bc" = "u")),
      # Calculate Percentages instead of absolute differences
      perc_low = (val / rec_low) * 100,
      perc_high = (val / rec_high) * 100
    )
  
  # 4. Generate wide segments
  wide_intake <- profile_clean %>%
    dplyr::select(name_en, val) %>%
    tidyr::pivot_wider(names_from = name_en, values_from = val, names_prefix = "total_daily_")
  
  wide_rec <- profile_clean %>%
    dplyr::select(name_en, rec_low, rec_high) %>%
    tidyr::pivot_wider(names_from = name_en, values_from = c(rec_low, rec_high), 
                       names_glue = "{.value}_{name_en}")
  
  wide_perc <- profile_clean %>%
    dplyr::select(name_en, perc_low, perc_high) %>%
    tidyr::pivot_wider(names_from = name_en, values_from = c(perc_low, perc_high), 
                       names_glue = "{.value}_{name_en}")
  
  # Combine initially
  combined <- dplyr::bind_cols(H, wide_intake, wide_rec, wide_perc) %>%
    janitor::clean_names()
  
  # --- 5. THE SORTING ENGINE ---
  nutrients <- unique(profile_clean$name_en)
  ordered_cols <- c("code", "age", "sex", "activity_cal") 
  
  for (nut in nutrients) {
    nut_pattern <- c(
      paste0("total_daily_", nut),
      paste0("rec_low_", nut),
      paste0("rec_high_", nut),
      paste0("perc_low_", nut),
      paste0("perc_high_", nut)
    )
    ordered_cols <- c(ordered_cols, intersect(nut_pattern, colnames(combined)))
  }
  
  final_row <- combined %>%
    dplyr::select(dplyr::all_of(ordered_cols))
  
  return(final_row)
}
