#' Batch Process a Directory of Dietary Recall Files
#'
#' @description
#' Automates the entire data pipeline for a study. This function scans a folder 
#' for recall files, processes them, and exports a "Results Package" containing 
#' the master dataset, a codebook for variable meanings, and a README audit file.
#'
#' @param folder_path String. Root directory containing participant files (.xls, .xlsx, .csv).
#' @param output_folder String. Name of the folder to create for results. Defaults to "Diet_Analysis_Results".
#' @param format String. "euro" (semicolon/comma) or "us" (comma/dot). Defaults to "us".
#'
#' @details
#' \strong{Exported Files:}
#' \itemize{
#'   \item \code{analysis_data.csv}: The master dataset with all participants.
#'   \item \code{codebook.csv}: Automatic dictionary mapping variable names to units and descriptions.
#'   \item \code{README.txt}: Technical summary, software versions, and legal disclaimer.
#' }
#'
#' @return A master data frame (tibble) returned invisibly.
#' @importFrom dplyr mutate tibble case_when
#' @importFrom purrr imap_dfr
#' @importFrom data.table fwrite
#' @importFrom utils txtProgressBar setTxtProgressBar write.csv packageVersion
#' @importFrom magrittr %>%
#' @export
process_dataset <- function(folder_path, output_folder = "Diet_Analysis_Results", format = "us") {
  
  # 1. Discovery
  files <- list.files(path = folder_path, pattern = "\\.(xls|xlsx|csv)$", 
                      full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  
  if (length(files) == 0) stop("No compatible files found.")
  
  message(sprintf("Found %d files. Processing...", length(files)))
  pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
  
  # 2. Batch Processing Loop
  master_table <- purrr::imap_dfr(files, function(file, i) {
    utils::setTxtProgressBar(pb, i)
    
    tryCatch({
      raw_data <- import_recall_data(file)
      if (nrow(raw_data$D) == 0) return(dplyr::tibble(source_file = basename(file), processing_status = "Skipped"))
      
      analysis <- analyze_diet(raw_data$D)
      flat_row <- cbind(list2DF(raw_data$H, nrow = 1), analysis)
      
      flat_row %>%
        mutate(source_file = basename(file), processing_status = "Success", .before = 1)
      
    }, error = function(e) {
      dplyr::tibble(source_file = basename(file), processing_status = "Failed", error_message = as.character(e))
    })
  })
  close(pb)
  
  # 3. Handle Exporting (The "Results Package" Logic)
  if (!is.null(output_folder)) {
    if (!dir.exists(output_folder)) dir.create(output_folder)
    
    # --- A. Save Data ---
    write_sep <- if(format == "euro") ";" else ","
    write_dec <- if(format == "euro") "," else "."
    data.table::fwrite(master_table, file.path(output_folder, "analysis_data.csv"), 
                       sep = write_sep, dec = write_dec)
    
    # --- B. Create Codebook ---
    # We use regex to identify units and descriptions based on janitor-cleaned names
    codebook <- data.frame(variable = colnames(master_table), stringsAsFactors = FALSE) %>%
      mutate(
        description = case_when(
          variable == "source_file" ~ "Original filename of the dietary recall",
          grepl("^total_daily_", variable) ~ "Mean daily intake (average of all recorded days)",
          grepl("^perc_kcal_", variable) ~ "Percentage of total daily energy contributed by this macronutrient",
          grepl("_perc_daily$", variable) ~ "Percentage of total daily energy contributed by this NOVA group",
          grepl("_nova[1-4]_perc$", variable) ~ "Percentage of this meal's energy contributed by this NOVA group",
          grepl("_nova[1-4]_kcal$", variable) ~ "Absolute energy (kcal) for this Meal/NOVA intersection",
          TRUE ~ "Participant metadata or specific nutrient value"
        ),
        unit = case_when(
          grepl("_perc", variable) ~ "%",
          grepl("_kcal$|energy", variable) ~ "kcal",
          grepl("vitamin_a|vitamin_d|folate|iodide|vitamin_b12|selenium", variable) ~ "ug",
          grepl("protein|fat|carbohydrate|sugar|fiber|alcohol|water", variable) ~ "g",
          grepl("age", variable) ~ "years",
          TRUE ~ "mg/units" # Default for most minerals and vitamins
        )
      )
    write.csv(codebook, file.path(output_folder, "codebook.csv"), row.names = FALSE)
    
    # --- C. Create README ---
    pkg_ver <- tryCatch(as.character(packageVersion("evalfinutTools")), error = function(e) "Development")
    readme_text <- c(
      "DIETARY ANALYSIS REPORT - evalfinutTools",
      paste("Generated on:", Sys.Date()),
      "------------------------------------------",
      paste("Software:", R.version$version.string),
      paste("Package: evalfinutTools v", pkg_ver),
      "------------------------------------------",
      "DISCLAIMER:",
      "This data is for research purposes only. Developers are not responsible",
      "for clinical decisions made based on these outputs.",
      "Methodology: Mean of daily totals with 0-imputation for skipped meals."
    )
    writeLines(readme_text, file.path(output_folder, "README.txt"))
    
    message(sprintf("\nResults exported to: %s/", output_folder))
  }
  
  invisible(master_table)
}