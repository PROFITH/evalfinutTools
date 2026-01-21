#' Import and Parse 24h Recall Data (Excel)
#'
#' @description
#' Reads a specifically formatted Excel file (Metabol format) containing both 
#' participant metadata (in the first row) and dietary recall data (starting from the fourth row).
#'
#' @param file String. Path to the input file. Must be an \code{.xls} or \code{.xlsx} file.
#'
#' @details
#' \strong{File Structure Expectation:}
#' This function expects a specific layout often used in "Metabol" exports:
#' \itemize{
#'   \item \strong{Row 1:} Participant metadata (Code, Name, Family Name, Sex, Age, Resting EE).
#'   \item \strong{Rows 2-3:} Skipped (usually empty or decorative).
#'   \item \strong{Row 4:} The header for the actual data table.
#' }
#' 
#' \strong{Required Columns in Data Table:}
#' The function explicitly looks for Spanish column headers:
#' \code{"Cód. Alimento"}, \code{"Alimento"}, \code{"Cantidad (g)"}, 
#' \code{"Comida"}, and \code{"Día"}.
#'
#' @return A named list containing two elements:
#' \item{H}{A list of participant metadata:
#'   \itemize{
#'     \item \code{code}: Participant ID.
#'     \item \code{name}: First name.
#'     \item \code{family_name}: Last name.
#'     \item \code{sex}: Gender.
#'     \item \code{age}: Age in years.
#'     \item \code{estimated_resting_ee}: Estimated resting energy expenditure.
#'   }
#' }
#' \item{D}{A clean data frame (tibble) containing the diet records with standardized English column names:
#'   \itemize{
#'     \item \code{food_id} (numeric)
#'     \item \code{food_name} (character)
#'     \item \code{serving_g} (numeric)
#'     \item \code{meal} (character)
#'     \item \code{day} (numeric)
#'   }
#' }
#'
#' @importFrom readxl read_xls read_xlsx
#' @importFrom tools file_ext
#' @export
import_recall_data <- function(file) {
  
  format = tools::file_ext(file)
  if (format == "xls") {
    # Header: first line in file
    header = readxl::read_xls(file, sheet = 1, n_max = 1)
    H = list(code = header[[1]],
             name = header[[2]],
             family_name = header[[3]],
             sex = header[[4]],
             age = header[[5]],
             estimated_resting_ee = header[[6]])
    
    # Data (including read_xls names): row 3 onwards
    data = readxl::read_xls(file, sheet = 1, skip = 2)
  } else if (format == "xlsx") {
    # Header: first line in file
    header = readxl::read_xlsx(file, sheet = 1, n_max = 1)
    H = list(code = header[[1]],
             name = header[[2]],
             family_name = header[[3]],
             sex = header[[4]],
             age = header[[5]],
             estimated_resting_ee = header[[6]])
    
    # Data (including read_xls names): row 3 onwards
    data = readxl::read_xlsx(file, sheet = 1, skip = 2)
  }
  
  
  # The file has metadata in the first 3 rows. The header is on row 4.
  # We skip 3 lines to get to the header.
  data = data[, c("Cód. Alimento", "Alimento", "Cantidad (g)", "Comida", "Día")]
  colnames(data) = c("food_id", "food_name", "serving_g", "meal", "day")
  
  # Ensure food_id is numeric (remove any non-numeric chars just in case)
  data$food_id = as.numeric(data$food_id)
  
  # Remove empty rows
  data = data[!is.na(data$food_id),]
  
  return(list(H = H, D = data))
}