#' Batch Process a Directory of Dietary Recall Files
#'
#' @description
#' Automates the entire data pipeline for a study. This function recursively searches a 
#' folder for all compatible dietary recall files (.xls, .xlsx, .csv), processes them individually 
#' to calculate NOVA and nutrient statistics, and merges them into a single master dataset.
#' It includes robust error handling and a real-time progress bar.
#'
#' @param folder_path String. The root directory containing the participant files. 
#'   The function searches recursively (i.e., it will find files inside subfolders).
#' @param output_csv String (Optional). The file path where the final results will be saved.
#'   Defaults to \code{"diet.csv"}. Set to \code{NULL} to skip saving to disk.
#'
#' @details
#' \strong{Workflow:}
#' \enumerate{
#'   \item \strong{Discovery:} Scans \code{folder_path} for all files matching .xls, .xlsx, or .csv (case insensitive).
#'   \item \strong{Processing:} Iterates through each file:
#'     \itemize{
#'       \item Parses metadata (Age, Sex, etc.) and recall data using \code{\link{import_recall_data}}.
#'       \item Calculates diet quality and NOVA metrics using \code{\link{analyze_diet}}.
#'     }
#'   \item \strong{Merging:} Combines participant metadata (header info) with their calculated diet statistics into a single wide-format row.
#'   \item \strong{Output:} Writes the final merged data frame to a CSV file using \code{data.table::fwrite} for speed.
#' }
#' 
#' \strong{Error Handling:}
#' If a specific file is corrupted or empty, the function will catch the error, print a warning 
#' to the console, and continue processing the remaining files without crashing.
#'
#' @return A master data frame (tibble) where each row represents one participant. 
#'   The columns include:
#'   \itemize{
#'     \item \code{source_file}: Name of the original file.
#'     \item \code{code, name, sex, age...}: Participant metadata from the file header.
#'     \item \code{total_daily_kcal}: Mean daily energy intake.
#'     \item \code{[meal]_nova[1-4]_kcal}: Detailed diet composition columns.
#'   }
#'   The function returns this data frame invisibly.
#'
#' @import dplyr
#' @import readr
#' @import purrr
#' @importFrom data.table fwrite
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
process_dataset <- function(folder_path, output_csv = "diet.csv") {
  
  # 1. Get list of files (Case insensitive for .xls, .xlsx, .csv)
  files <- list.files(path = folder_path, pattern = "\\.(xls|xlsx|csv)$", 
                      full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  
  if (length(files) == 0) {
    stop("No compatible files (.xls, .xlsx, .csv) found in the specified folder.")
  }
  
  message(sprintf("Found %d files. Starting batch processing...", length(files)))
  
  # 2. Iterate safely
  
  # --- INIT PROGRESS BAR ---
  pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
  
  # We use map_dfr from purrr (part of tidyverse) to loop and bind rows automatically
  master_table <- purrr::imap_dfr(files, function(file, i) {
    
    # Update the progress bar 
    utils::setTxtProgressBar(pb, i)
    
    # Try-catch block prevents one bad file from crashing the whole script
    tryCatch({
      
      # A. Import
      raw_data <- import_recall_data(file)
      
      # Check if empty
      if (nrow(raw_data$D) == 0) {
        warning(paste("Skipping empty file:", basename(file)))
        return(NULL)
      }
      
      # B. Analyze
      # We only care about the 'summary' part (the flat row)
      analysis <- analyze_diet(raw_data$D)
      flat_row <- cbind(list2DF(raw_data$H, nrow = 1), analysis)
      
      # C. Add Metadata
      # It's useful to know which filename this data came from
      flat_row <- flat_row %>%
        mutate(source_file = file, .before = 1)
      
      return(flat_row)
      
    }, error = function(e) {
      # On error, print message and return NULL (skipping row)
      message(paste("ERROR in file:", basename(file), "-", e$message))
      return(NULL)
    })
  })
  
  # Close the progress bar
  close(pb)
  
  # 3. Final Summary
  message(sprintf("\nBatch complete. Successfully processed %d out of %d files.", 
                  nrow(master_table), length(files)))
  
  # 4. Save to CSV if requested
  if (!is.null(output_csv)) {
    data.table::fwrite(master_table, output_csv)
    message(paste("Results saved to:", output_csv))
  }
  
  invisible(master_table)
}