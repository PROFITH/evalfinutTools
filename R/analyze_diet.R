#' Calculate Comprehensive Diet Quality and NOVA Statistics
#'
#' @description
#' Calculates total energy intake and examines the distribution of calories across 
#' different meals and NOVA food processing classifications. The function aggregates 
#' 24-hour recall data, imputes missing values (e.g., skipped meals count as 0 kcal), 
#' and returns a flattened, single-row summary suitable for regression analysis.
#'
#' @param recall_data A data frame containing the dietary recall data, typically generated 
#'   by \code{\link{import_recall_data}}. If supplied manually, it must contain the 
#'   following columns:
#'   \itemize{
#'     \item \code{food_id}: Unique identifier linking to the internal food database.
#'     \item \code{serving_g}: Portion size in grams.
#'     \item \code{day}: The day of the record (e.g., 1, 2, 3).
#'     \item \code{meal}: Name of the meal (e.g., "Desayuno", "Cena").
#'     \item \code{NOVA}: The NOVA group classification (1-4).
#'   }
#'
#' @details
#' \strong{Methodology:}
#' This function calculates the \emph{mean of daily totals}. For every participant, it:
#' \enumerate{
#'   \item Calculates the total energy (kcal) for every specific Day/Meal/NOVA combination.
#'   \item Imputes \code{0} for any combination not present (e.g., if a participant skipped "Cena" on Day 2, it is recorded as 0 kcal rather than missing).
#'   \item Averages these daily sums to produce a representative daily intake.
#' }
#' 
#' 
#'
#' \strong{Output Structure:}
#' The output is a "wide" format data frame with one row per participant. Column names are 
#' standardized to snake_case using \code{janitor::clean_names()}. The columns follow this pattern:
#' \itemize{
#'   \item \code{total_daily_kcal}: Grand mean of daily energy intake.
#'   \item \code{[meal]_kcal}: Absolute energy for a specific meal (e.g., \code{desayuno_kcal}).
#'   \item \code{[meal]_perc}: Percent of total daily energy from that meal.
#'   \item \code{nova[1-4]_kcal}: Absolute energy from a specific NOVA group.
#'   \item \code{nova[1-4]_perc}: Percent of total daily energy from that NOVA group.
#'   \item \code{[meal]_nova[1-4]_kcal}: Intersection of Meal and NOVA (e.g., \code{cena_nova4_kcal}).
#' }
#'
#' @return A single-row data frame (tibble) containing flattened statistical summaries. 
#'   All column names are lower_snake_case.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom janitor clean_names
#' @export
#'
#' @examples
#' \dontrun{
#'   # Classic workflow
#'   raw_data <- import_recall_data("participant_001.xls")
#'   diet_stats <- analyze_diet(raw_data)
#'   
#'   # Access specific metrics
#'   print(diet_stats$total_daily_kcal)
#'   print(diet_stats$cena_nova4_perc) # % of calories from UPF at dinner
#' }
analyze_diet <- function(recall_data) {
  
  # 1. Merge Recall with Internal Food DB
  merged_data <- dplyr::left_join(recall_data, food_db, by = c("food_id" = "id"))
  
  # 2. Validation
  missing_count <- sum(is.na(merged_data$food_name))
  if (missing_count > 0) {
    warning(sprintf("%d items found in recall with no matching Food ID in database.", missing_count))
  }
  
  # 3. Calculations (Ensure columns match your data, e.g., grams vs serving_g)
  results <- merged_data %>%
    mutate(
      kcal = (serving_g / 100) * energy 
    )
  
  # Complete missing meals/NOVA in the raw data (Optional but good for day-sums)
  results <- results %>%
    tidyr::complete(day, meal, fill = list(kcal = 0)) %>%
    tidyr::complete(day, NOVA, fill = list(kcal = 0))
  
  # Factorize meal and nova
  results$meal = factor(results$meal, 
                        levels = c("Desayuno", "Media mañana", "Almuerzo",
                                   "Merienda", "Cena"),
                        labels = c("desayuno", "media manana", "almuerzo",
                                   "merienda", "cena"))

  # 4. Aggregations
  
  # Energy per day
  kcal_day <- aggregate(kcal ~ day, data = results, FUN = sum, na.rm = TRUE)
  kcal_mean <- mean(kcal_day[,2])
  
  # Energy per meal
  kcal_meal_day <- aggregate(kcal ~ meal*day, data = results, FUN = sum, na.rm = TRUE)
  kcal_meal_mean <- aggregate(kcal ~ meal, data = kcal_meal_day, FUN = mean, na.rm = TRUE)
  kcal_meal_mean$perc <- kcal_meal_mean$kcal / kcal_mean * 100
  
  # Energy per NOVA
  kcal_nova_day <- aggregate(kcal ~ NOVA*day, data = results, FUN = sum, na.rm = TRUE)
  kcal_nova_mean <- aggregate(kcal ~ NOVA, data = kcal_nova_day, FUN = mean, na.rm = TRUE)
  kcal_nova_mean$perc <- kcal_nova_mean$kcal / kcal_mean * 100
  
  # Energy per NOVA * Meal
  kcal_nova_meal_day <- aggregate(kcal ~ NOVA*meal*day, data = results, FUN = sum, na.rm = TRUE)
  kcal_nova_meal_mean <- aggregate(kcal ~ NOVA*meal, data = kcal_nova_meal_day, FUN = mean, na.rm = TRUE)
  
  # Merge with meal totals to get %
  kcal_nova_meal_mean <- merge(
    kcal_nova_meal_mean,
    kcal_meal_mean[, c("meal", "kcal")],
    by = "meal",
    suffixes = c("", "_total_meal")
  )
  kcal_nova_meal_mean$perc <- kcal_nova_meal_mean$kcal / kcal_nova_meal_mean$kcal_total_meal * 100
  
  # --- 4.5. THE FIX: Force Missing Combinations ---
  # This ensures "Desayuno NOVA 1" exists as 0 even if never consumed.
  kcal_nova_meal_mean <- kcal_nova_meal_mean %>%
    tidyr::as_tibble() %>%
    tidyr::complete(
      meal, 
      NOVA = c(1, 2, 3, 4), # Explicitly look for groups 1, 2, 3, 4
      fill = list(kcal = 0, perc = 0)
    )
  
  # 5. Flattening (Structure)
  
  # Total
  row_total <- data.frame(total_daily_kcal = as.numeric(kcal_mean))
  
  # By Meal
  row_meal <- kcal_meal_mean %>%
    tidyr::pivot_wider(
      names_from = meal, 
      values_from = c(kcal, perc),
      names_glue = "{meal}_{.value}"
    )
  
  # By NOVA
  row_nova <- kcal_nova_mean %>%
    tidyr::pivot_wider(
      names_from = NOVA,
      values_from = c(kcal, perc),
      names_glue = "NOVA{NOVA}_{.value}"
    )
  
  # By NOVA + Meal
  row_nova_meal <- kcal_nova_meal_mean %>%
    dplyr::mutate(combo_label = paste0(meal, "_NOVA", NOVA)) %>%
    # Select ONLY the columns to pivot so we get 1 row
    dplyr::select(combo_label, kcal, perc) %>%
    tidyr::pivot_wider(
      names_from = combo_label,
      values_from = c(kcal, perc),
      names_glue = "{combo_label}_{.value}"
    )
  
  # Combine
  final_single_row <- dplyr::bind_cols(row_total, row_meal, row_nova, row_nova_meal)
  final_single_row = janitor::clean_names(final_single_row)
  
  # Return the correct object
  return(final_single_row)
}