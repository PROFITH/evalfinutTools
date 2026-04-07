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
process_recall_24h <- function(folder_path, output_folder = "Diet_Analysis_Results", format = "us") {
  
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
      
      analysis <- analyze_recalls(raw_data$D)
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
      "==========================================================================",
      "             RAW 24-HOUR RECALL ANALYSIS - evalfinutTools                 ",
      "==========================================================================",
      paste("Generated on: ", Sys.Date()),
      paste("Software:     ", R.version$version.string),
      paste("Package:      evalfinutTools v", pkg_ver),
      "--------------------------------------------------------------------------",
      "",
      "1. FOLDER CONTENTS",
      "   - analysis_data.csv: The master dataset containing all participants.",
      "   - codebook.csv:      A dictionary defining every variable and its units.",
      "   - README.txt:        This navigation guide.",
      "",
      "2. DATA STRUCTURE & COLUMN PREFIXES",
      "   This dataset calculates nutrient intake at three different levels:",
      "",
      "   A. DAILY TOTALS (Prefix: total_daily_)",
      "      The grand mean of intake across all recorded days.",
      "      Example: 'total_daily_energy_kcal' is the average daily calories.",
      "",
      "   B. BY MEAL (Prefixes: desayuno_, almuerzo_, etc.)",
      "      The average intake contributed by specific meals.",
      "      Example: 'cena_protein_total_g' is the mean protein intake at dinner.",
      "",
      "   C. BY NOVA GROUP (Prefixes: nova1_, nova2_, nova3_, nova4_)",
      "      Intake categorized by the NOVA food processing classification.",
      "      Example: 'nova4_energy_kcal' is the energy from ultra-processed foods.",
      "",
      "3. PERCENTAGE & RATIO COLUMNS",
      "   - perc_kcal_[macro]: Percentage of total daily energy from protein, fat, etc.",
      "     Calculated using Atwater factors (4-4-9-7-2).",
      "   - nova[1-4]_perc_daily: The % of total daily calories from that NOVA group.",
      "   - [meal]_nova[1-4]_perc: The % of that specific meal's calories from that",
      "     NOVA group (e.g., 'merienda_nova4_perc' is the % of UPF at snack time).",
      "",
      "4. METHODOLOGY & DATA CLEANING",
      "   - ZERO IMPUTATION: If a participant skipped a meal or did not consume a",
      "     specific NOVA group, it is recorded as 0. This ensures the 'Mean of Daily",
      "     Totals' is not artificially inflated by missing records.",
      "   - UNIT STANDARDIZATION: All nutrients include their units in the column",
      "     name (g, mg, ug, kcal) to ensure clarity for statistical modeling.",
      "   - DATABASE: Calculations are performed using the BEDCA food composition",
      "     reference via the internal 'food_db'.",
      "",
      "5. LEGAL DISCLAIMER",
      "   This data was batch-processed automatically. Researchers should manually",
      "   verify outliers. The developers of 'evalfinutTools' are not responsible",
      "   for clinical or research interpretations derived from this output.",
      "=========================================================================="
    )
    writeLines(readme_text, file.path(output_folder, "README.txt"))
    
    message(sprintf("\nResults exported to: %s/", output_folder))
  }
  
  invisible(master_table)
}