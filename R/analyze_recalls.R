#' Calculate Comprehensive Diet Quality, Nutrient Profiles, and NOVA Statistics
#'
#' @description
#' Calculates mean daily intake for the full suite of BEDCA nutrients (energy through zinc) 
#' and examines their distribution across different meals and NOVA food processing 
#' classifications. The function cleans raw character data, imputes missing values 
#' (skipped meals/missing DB entries count as 0), and returns a flattened, 
#' single-row summary suitable for regression analysis.
#'
#' @param recall_data A data frame containing the dietary recall data, typically generated 
#'   by \code{\link{import_recall_data}}. It must contain:
#'   \itemize{
#'     \item \code{food_id}: Unique identifier linking to \code{food_db}.
#'     \item \code{serving_g}: Portion size consumed in grams.
#'     \item \code{day}: The record day (e.g., 1, 2, 3).
#'     \item \code{meal}: Name of the meal (e.g., "Desayuno", "Almuerzo").
#'   }
#' @param food_db A data frame containing the BEDCA food composition database. 
#'   Must include \code{id}, \code{NOVA}, and nutrient columns from \code{energy} 
#'   to \code{zinc}.
#'
#' @details
#' \strong{Methodology:}
#' \enumerate{
#'   \item \strong{Data Cleaning:} Converts character-based nutrient strings to numeric 
#'   and replaces empty strings or NAs with \code{0}.
#'   \item \strong{Scaling:} All nutrients are scaled by \code{(serving_g / 100)}.
#'   \item \strong{Imputation:} Uses a complete grid of all Day/Meal/NOVA combinations 
#'   to ensure that skipped meals are correctly calculated as \code{0} rather than 
#'   missing in the final means.
#'   \item \strong{Averaging:} Calculates the \emph{mean of daily totals}. For each 
#'   nutrient, it sums the intake per day and then averages those daily totals 
#'   across the recording period.
#' }
#' 
#' \strong{Macro-Energy Ratios:}
#' Calculates the percentage of total daily energy derived from major sources using 
#' standard Atwater factors: Protein (4 kcal/g), Carbohydrates (4 kcal/g), 
#' Fats (9 kcal/g), Alcohol (7 kcal/g), and Fiber (2 kcal/g).
#'
#' \strong{Output Structure:}
#' The output is a "wide" tibble with one row. Column names are standardized 
#' to snake_case. Patterns include:
#' \itemize{
#'   \item \code{total_daily_[nutrient]}: Grand mean of daily intake for all nutrients.
#'   \item \code{perc_kcal_[macro]}: % of total daily energy from protein, fat, carbs, etc.
#'   \item \code{[meal]_[nutrient]}: Absolute nutrient intake during a specific meal.
#'   \item \code{nova[1-4]_[nutrient]}: Absolute nutrient intake from a specific NOVA group.
#'   \item \code{[meal]_nova[1-4]_kcal}: Absolute energy at the intersection of meal and NOVA.
#'   \item \code{nova[1-4]_perc_daily}: % of total daily energy from that NOVA group.
#'   \item \code{[meal]_nova[1-4]_perc}: % of a specific meal's energy from that NOVA group.
#' }
#'
#' @return A single-row data frame (tibble).
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang !! sym
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @export
#'
#' @examples
#' \dontrun{
#'   # Analysis of 24h recall using internal food database
#'   diet_stats <- analyze_recalls(recall_data = participant_data, food_db = bedca_db)
#'   
#'   # Access total energy and NOVA 4 (ultraprocessed food) contribution
#'   diet_stats$total_daily_energy
#'   diet_stats$nova4_perc_daily
#'   diet_stats$almuerzo_nova4_perc
#' }
analyze_recalls <- function(recall_data) {
  
  # 1. Merge Recall with Internal Food DB
  merged_data <- dplyr::left_join(recall_data, food_db, by = c("food_id" = "id"), 
                                  keep = TRUE)
  
  # 2. Validation
  missing_count <- sum(is.na(merged_data$food_name))
  if (missing_count > 0) {
    warning(sprintf("%d items found in recall with no matching Food ID in database.", missing_count))
  }
  
  # 3. Pre-processing: Clean Database Characters & Replace DB NAs
  nutrient_cols_base <- colnames(food_db)[12:54]
  
  results <- merged_data %>%
    mutate(across(all_of(nutrient_cols_base), ~ {
      # 1. Convert to character
      x <- as.character(.x)
      
      # 2. Clean common non-numeric symbols found in BEDCA/Metabol
      # Replaces "<...", "traces", "tr", "N.D." with "0"
      x <- gsub("<.*|traces|tr|N\\.D\\.|n\\.d\\.|\\s+", "0", x)
      
      # 3. Suppress the warning only for the final numeric conversion
      suppressWarnings(as.numeric(x))
    })) %>%
    mutate(across(all_of(nutrient_cols_base), ~ tidyr::replace_na(.x, 0)))
  
  # 4. Expansion: Complete missing meals/NOVA in the raw data
  grid <- expand.grid(
    day = unique(results$day),
    meal = c("Desayuno", "Media mañana", "Almuerzo", "Merienda", "Cena"),
    NOVA = 1:4
  )
  
  # Merge data and grid
  # IMPORTANT: We use left_join or merge(all.x=T), then immediately zero-out NAs
  results_complete <- merge(grid, results, by = c("day", "meal", "NOVA"), all.x = TRUE)
  
  # 5. Calculation & Unit Renaming
  results_complete <- results_complete %>%
    mutate(
      serving_g = tidyr::replace_na(serving_g, 0),
      across(all_of(nutrient_cols_base), ~ tidyr::replace_na(.x, 0))
    ) %>%
    # Perform Gram-scaling
    mutate(across(all_of(nutrient_cols_base), ~ (serving_g / 100) * .x)) %>%
    # Harmonized English Renaming
    rename_with(.cols = all_of(nutrient_cols_base), .fn = function(x) {
      new_name <- case_when(
        x == "energy" ~ "energy",
        x == "proteinTotal" ~ "protein",
        x == "fatTotal" ~ "fat_total",
        x == "carbohydrates" ~ "carbohydrates",
        x == "sugar" ~ "sugar",
        x == "fiberTotal" ~ "fiber",
        x == "alcohol" ~ "alcohol",
        x == "water" ~ "water",
        x == "cholesterol" ~ "cholesterol",
        x == "vitaminA" ~ "vitamin_a",
        x == "vitaminD" ~ "vitamin_d",
        x == "vitaminE" ~ "vitamin_e",
        x == "vitaminC" ~ "vitamin_c",
        x == "folateTotal" ~ "folate",
        x == "biotin" ~ "biotin",
        x == "tiamin" ~ "thiamin",
        x == "riboflavin" ~ "riboflavin",
        x == "niacinEqTotal" ~ "niacin",
        x == "vitaminB5" ~ "vitamin_b5",
        x == "vitaminB6Total" ~ "vitamin_b6",
        x == "vitaminB12" ~ "vitamin_b12",
        x == "calcium" ~ "calcium",
        x == "ironTotal" ~ "iron",
        x == "potassium" ~ "potassium",
        x == "magnesium" ~ "magnesium",
        x == "sodium" ~ "sodium",
        x == "phosphorus" ~ "phosphorus",
        x == "iodide" ~ "iodide",
        x == "seleniumTotal" ~ "selenium",
        x == "zinc" ~ "zinc",
        grepl("^fattyAcid", x) ~ janitor::make_clean_names(gsub("fattyAcid", "fatty_acid_", x)),
        TRUE ~ x
      )
      
      # Append Unit
      case_when(
        new_name == "energy" ~ paste0(new_name, "_kcal"),
        new_name %in% c("vitamin_a", "vitamin_d", "folate", "biotin", "vitamin_b12", "iodide", "selenium") ~ paste0(new_name, "_ug"),
        new_name %in% c("iron", "calcium", "potassium", "magnesium", "sodium", "phosphorus", "zinc", 
                        "cholesterol", "vitamin_e", "vitamin_c", "niacin", "vitamin_b5", "riboflavin", "thiamin", "vitamin_b6") ~ paste0(new_name, "_mg"),
        TRUE ~ paste0(new_name, "_g")
      )
    }) %>%
    mutate(meal = factor(meal, 
                         levels = c("Desayuno", "Media mañana", "Almuerzo", "Merienda", "Cena"),
                         labels = c("desayuno", "media_manana", "almuerzo", "merienda", "cena")))
  
  # Update nutrient_cols to the now-renamed columns
  nutrient_cols <- grep("_g$|_mg$|_ug$|_kcal$", colnames(results_complete), value = TRUE)
  
  # --- Aggregations (Mean of Daily Totals) ---
  
  # A. TOTAL DAILY
  daily_sums <- results_complete %>%
    group_by(day) %>%
    summarise(across(all_of(nutrient_cols), sum), .groups = "drop")
  
  daily_totals <- daily_sums %>%
    summarise(across(all_of(nutrient_cols), mean)) %>%
    rename_with(~ paste0("total_daily_", .x))
  
  # B. BY MEAL
  meal_stats <- results_complete %>%
    group_by(day, meal) %>%
    summarise(across(all_of(nutrient_cols), sum), .groups = "drop") %>%
    group_by(meal) %>%
    summarise(across(all_of(nutrient_cols), mean), .groups = "drop")
  
  # C. BY NOVA
  nova_stats <- results_complete %>%
    group_by(day, NOVA) %>%
    summarise(across(all_of(nutrient_cols), sum), .groups = "drop") %>%
    group_by(NOVA) %>%
    summarise(across(all_of(nutrient_cols), mean), .groups = "drop")
  
  # D. INTERACTION: MEAL x NOVA (Using Energy specifically)
  energy_col <- grep("energy_kcal", nutrient_cols, value = TRUE)
  meal_nova_interaction <- results_complete %>%
    group_by(day, meal, NOVA) %>%
    summarise(energy = sum(!!sym(energy_col)), .groups = "drop") %>%
    group_by(meal, NOVA) %>%
    summarise(mean_energy = mean(energy), .groups = "drop")
  
  # --- 6. PERCENTAGE CALCULATIONS ---
  
  # Logic updated to use the new Unit-Suffixed column names
  macro_perc <- daily_totals %>%
    mutate(
      perc_kcal_protein = (total_daily_protein_g * 4) / total_daily_energy_kcal * 100,
      perc_kcal_carbs   = (total_daily_carbohydrates_g * 4) / total_daily_energy_kcal * 100,
      perc_kcal_fat     = (total_daily_fat_total_g * 9) / total_daily_energy_kcal * 100,
      perc_kcal_alcohol = (total_daily_alcohol_g * 7) / total_daily_energy_kcal * 100,
      perc_kcal_fiber   = (total_daily_fiber_g * 2) / total_daily_energy_kcal * 100,
      perc_kcal_residual = 100 - (perc_kcal_protein + perc_kcal_carbs + 
                                    perc_kcal_fat + perc_kcal_alcohol + perc_kcal_fiber)
    ) %>%
    dplyr::select(starts_with("perc_kcal"))
  
  # II. NOVA % of Total Daily Kcal
  nova_daily_perc <- nova_stats %>%
    mutate(perc_total_daily_kcal = !!sym(energy_col) / sum(nova_stats[[energy_col]]) * 100) %>%
    mutate(label = paste0("nova", NOVA, "_perc_daily")) %>%
    dplyr::select(label, perc_total_daily_kcal) %>%
    tidyr::pivot_wider(names_from = label, values_from = perc_total_daily_kcal)
  
  # III. NOVA % of Specific Meal Kcal
  meal_nova_perc <- meal_nova_interaction %>%
    left_join(meal_stats %>% dplyr::select(meal, total_meal_energy = !!sym(energy_col)), by = "meal") %>%
    mutate(perc_of_meal_kcal = (mean_energy / total_meal_energy) * 100) %>%
    mutate(perc_of_meal_kcal = tidyr::replace_na(perc_of_meal_kcal, 0)) %>%
    mutate(label = paste0(meal, "_nova", NOVA, "_perc")) %>%
    dplyr::select(label, perc_of_meal_kcal) %>%
    tidyr::pivot_wider(names_from = label, values_from = perc_of_meal_kcal)
  
  # --- FLATTENING AND FINAL JOIN ---
  
  meal_stats_wide <- meal_stats %>%
    tidyr::pivot_wider(names_from = meal, values_from = all_of(nutrient_cols), names_glue = "{meal}_{.value}")
  
  nova_stats_wide <- nova_stats %>%
    mutate(NOVA = paste0("nova", NOVA)) %>%
    tidyr::pivot_wider(names_from = NOVA, values_from = all_of(nutrient_cols), names_glue = "{NOVA}_{.value}")
  
  interaction_wide <- meal_nova_interaction %>%
    mutate(label = paste0(meal, "_nova", NOVA, "_kcal")) %>%
    dplyr::select(label, mean_energy) %>%
    tidyr::pivot_wider(names_from = label, values_from = mean_energy)
  
  final_row <- dplyr::bind_cols(
    daily_totals, macro_perc, meal_stats_wide, nova_stats_wide, 
    interaction_wide, nova_daily_perc, meal_nova_perc
  ) %>%
    janitor::clean_names()
  
  return(final_row)
}