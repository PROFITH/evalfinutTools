#' Batch Process EvalFinut Intake Profiles
#'
#' @description
#' Automates the parsing of "Mean Intake Profile" Excel files. Extracts nutrient 
#' estimates and adequacy percentages. This version automatically cleans the 
#' output by removing empty recommendation columns and condensing identical 
#' Low/High ranges into a single column.
#'
#' @param folder_path String. Directory containing the summary .xlsx or .xls files.
#' @param output_folder String. Folder for results. Defaults to "Intake_Profile_Results".
#' @param format String. "us" (comma/dot) or "euro" (semicolon/comma). Defaults to "us".
#'
#' @return A master data frame (tibble) returned invisibly.
#' 
#' @importFrom dplyr mutate tibble bind_rows select where all_of rename
#' @importFrom purrr imap_dfr
#' @importFrom data.table fwrite
#' @importFrom utils txtProgressBar setTxtProgressBar write.csv packageVersion
#' @importFrom tidyselect starts_with
#' @export
process_intake_profiles <- function(folder_path, output_folder = "Intake_Profile_Results", format = "us") {
  
  # 1. Discovery
  files <- list.files(path = folder_path, pattern = "\\.(xls|xlsx)$", 
                      full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  
  if (length(files) == 0) stop("No summary files found.")
  
  message(sprintf("Found %d profile files. Processing...", length(files)))
  pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
  
  # 2. Batch Processing Loop
  master_table <- purrr::imap_dfr(files, function(file, i) {
    utils::setTxtProgressBar(pb, i)
    tryCatch({
      row_data <- import_intake_profile(file)
      row_data %>%
        dplyr::mutate(source_file = basename(file), processing_status = "Success", .before = 1)
    }, error = function(e) {
      dplyr::tibble(source_file = basename(file), processing_status = "Failed", error_message = as.character(e))
    })
  })
  close(pb)
  
  # --- 3. CLEAN-UP LOGIC ---
  
  # A. Delete columns that are missing (NA) for all participants
  master_table <- master_table %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x))))
  
  # B. Condense identical Low/High ranges
  # Get list of nutrients based on intake columns
  nutrients <- gsub("total_daily_", "", grep("^total_daily_", names(master_table), value = TRUE))
  
  for (nut in nutrients) {
    low_col   <- paste0("rec_low_", nut)
    high_col  <- paste0("rec_high_", nut)
    p_low_col <- paste0("perc_low_", nut)
    p_high_col <- paste0("perc_high_", nut)
    
    # Check if both low and high columns exist for this nutrient
    if (low_col %in% names(master_table) && high_col %in% names(master_table)) {
      
      # Determine if they are identical for ALL rows (ignoring NAs)
      is_identical <- all(master_table[[low_col]] == master_table[[high_col]], na.rm = TRUE)
      
      if (is_identical) {
        # Rename Low to single column and drop the High column
        master_table <- master_table %>%
          dplyr::rename(!!paste0("rec_", nut) := !!low_col,
                        !!paste0("perc_", nut) := !!p_low_col) %>%
          dplyr::select(-dplyr::all_of(c(high_col, p_high_col)))
      }
    }
  }
  
  # 4. Export Results Package
  if (!is.null(output_folder)) {
    if (!dir.exists(output_folder)) dir.create(output_folder)
    
    # Save Data
    write_sep <- if(format == "euro") ";" else ","
    write_dec <- if(format == "euro") "," else "."
    data.table::fwrite(master_table, file.path(output_folder, "intake_adequacy_data.csv"), 
                       sep = write_sep, dec = write_dec)
    
    # Create English Codebook
    codebook <- data.frame(variable = colnames(master_table), stringsAsFactors = FALSE) %>%
      dplyr::mutate(
        description = dplyr::case_when(
          variable == "source_file" ~ "Original file name",
          grepl("^total_daily_", variable) ~ "Nutrient intake estimate",
          grepl("^rec_", variable) ~ "Nutrient recommendation",
          grepl("^rec_low_", variable) ~ "Lower bound of recommended range",
          grepl("^rec_high_", variable) ~ "Upper bound of recommended range",
          grepl("^perc_", variable) ~ "Percentage of adequacy",
          grepl("^perc_low_", variable) ~ "Adequacy relative to lower recommendation",
          grepl("^perc_high_", variable) ~ "Adequacy relative to upper recommendation",
          TRUE ~ "Participant metadata"
        ),
        unit = dplyr::case_when(
          grepl("^perc", variable) ~ "%",
          grepl("energy|kcal", variable) ~ "kcal",
          grepl("_ug$|folate|vitamin_a|vitamin_d|vitamin_b12|iodide|selenium", variable) ~ "ug",
          grepl("age", variable) ~ "years",
          TRUE ~ "mg or g"
        )
      )
    utils::write.csv(codebook, file.path(output_folder, "codebook.csv"), row.names = FALSE)
    
    # Create README
    pkg_ver <- tryCatch(as.character(utils::packageVersion("evalfinutTools")), error = function(e) "0.1.0")
    readme_text <- c(
      "==========================================================================",
      "             EVALFINUT DIETARY PROFILE - ANALYSIS PACKAGE                 ",
      "==========================================================================",
      paste("Generated on:", Sys.Date()),
      paste("Software:   ", R.version$version.string),
      paste("Package:     evalfinutTools v", pkg_ver),
      "--------------------------------------------------------------------------",
      "",
      "1. FOLDER CONTENTS",
      "   - intake_adequacy_data.csv: The master dataset containing all participants.",
      "   - codebook.csv:             A dictionary defining every variable and its units.",
      "   - README.txt:               This navigation guide.",
      "",
      "2. HOW TO NAVIGATE THE DATA (analysis_data.csv)",
      "   The dataset is organized in 'Nutrient Blocks' for easy reading.",
      "   For each nutrient, you will find columns side-by-side in this order:",
      "",
      "   total_daily_[Nutrient]:   The actual amount the participant consumed.",
      "   rec_[Nutrient]:      The recommended target (PRI/RDA).",
      "   perc_[Nutrient]:     Percentage of adequacy (Intake / Rec * 100).",
      "",
      "3. UNDERSTANDING RANGES vs. SINGLE TARGETS",
      "   - SINGLE TARGETS: Most vitamins/minerals have one recommendation.",
      "     Example: 'rec_vitamin_c' and 'perc_vitamin_c'.",
      "   - RANGES: For macronutrients (Fats/Carbs), recommendations are ranges.",
      "     Example: 'rec_low_fat_total' (20% Energy) and 'rec_high_fat_total' (35% Energy).",
      "     In these cases, adequacy is provided for both bounds (perc_low and perc_high).",
      "",
      "4. DATA CLEANING & QUALITY AUDIT",
      "   - EMPTY COLUMNS: If a nutrient had no recommendation data across the entire",
      "     study group, the recommendation/percentage columns were removed to avoid clutter.",
      "   - RECYCLING: Skipped rows or unparseable data are marked as NA.",
      "   - METADATA: The first columns (code, age, sex, activity_cal) provide context.",
      "",
      "5. LEGAL DISCLAIMER",
      "   This data was batch-processed automatically using 'evalfinutTools'.",
      "   While every effort is made to ensure parsing accuracy from Excel exports,",
      "   researchers should manually verify outliers. Developers are not liable",
      "   for clinical or policy decisions made using this output.",
      "=========================================================================="
    )
    writeLines(readme_text, file.path(output_folder, "README.txt"))
    
    message(sprintf("\nResults exported to: %s/", output_folder))
  }
  
  invisible(master_table)
}
