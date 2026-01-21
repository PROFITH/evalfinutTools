#' Food Composition Database with NOVA Classification
#'
#' A dataset containing nutritional information and NOVA processing classification
#' for common food items, based on the BEDCA and client-specific data.
#'
#' @format A data frame with rows for food items and variables:
#' \describe{
#'   \item{food_id}{Unique identifier matching the recall data.}
#'   \item{food_name}{Descriptive name of the food.}
#'   \item{nova_group}{NOVA classification (1-4).}
#'   \item{kcal_per_100g}{Energy content in kcal per 100g.}
#'   \item{is_upf}{Logical flag indicating if the food is Ultra-Processed.}
#' }
#' @source Internal client database.
"food_db"