library(testthat)
library(dplyr)
library(evalfinutTools)

# ==========================================================================
# PIPELINE 1: RAW 24-HOUR RECALLS (Bottom-Up)
# ==========================================================================

# --- TEST 1.1: RECALL IMPORT ---
test_that("import_recall_data reads Metabol XLS/XLSX correctly", {
  
  # Path to the recalls subfolder
  file_path <- system.file("testdata", "recalls", "metabol1.xls", package = "evalfinutTools")
  
  result <- import_recall_data(file_path)
  
  expect_type(result, "list")
  expect_named(result, c("H", "D"))
  
  # Check Metadata
  expect_equal(as.character(result$H$name), "Metabol001_V0")
  
  # Check Data Cleaning
  df <- result$D
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 40)
  expect_equal(df$meal[1], "Desayuno")
  expect_type(df$food_id, "double")
})

# --- TEST 1.2: RECALL ANALYSIS (CALCULATIONS) ---
test_that("analyze_recalls computes correct energy and English units", {
  
  file_path <- system.file("testdata", "recalls", "metabol2.xlsx", package = "evalfinutTools")
  raw_data <- import_recall_data(file_path)$D
  
  # Run Analysis
  stats <- analyze_recalls(raw_data)
  
  expect_s3_class(stats, "data.frame")
  expect_equal(nrow(stats), 1) 
  
  # Check Harmonized Naming (intake_ vs total_daily_)
  expect_true("total_daily_energy_kcal" %in% names(stats))
  expect_true("total_daily_protein_g" %in% names(stats))
  
  # Check Imputation & Meal Distribution
  expect_true("desayuno_energy_kcal" %in% names(stats))
  
  # Check NOVA Logic
  expect_true("nova4_energy_kcal" %in% names(stats))
  expect_true("nova4_perc_daily" %in% names(stats))
})

# --- TEST 1.3: RECALL BATCH ---
test_that("process_recall_24h iterates through recalls subfolder", {
  
  batch_dir <- system.file("testdata", "recalls", package = "evalfinutTools")
  
  # Run Batch
  master_df <- suppressMessages(
    process_recall_24h(folder_path = batch_dir, output_folder = "Recall_Test_Results")
  )
  
  expect_equal(nrow(master_df), 2) # metabol1 and metabol2
  expect_true(all(c("total_daily_energy_kcal", "source_file") %in% names(master_df)))
  
  # Clean up
  if (dir.exists("Recall_Test_Results")) unlink("Recall_Test_Results", recursive = TRUE)
})


# ==========================================================================
# PIPELINE 2: SUMMARY INTAKE PROFILES (Top-Down)
# ==========================================================================

# --- TEST 2.1: PROFILE IMPORT ---
test_that("import_intake_profile parses ranges and English translations", {
  
  file_path <- system.file("testdata", "intake_profiles", "metabol1.xlsx", package = "evalfinutTools")
  
  profile <- import_intake_profile(file_path)
  
  expect_s3_class(profile, "data.frame")
  
  # Check metadata parsing
  expect_true(all(c("code", "age", "sex", "activity_cal") %in% names(profile)))
  
  # Check the Interleaved Order & Units (Energy is usually first)
  # Intake -> Rec Low -> Rec High -> Perc Low -> Perc High
  expect_true("intake_energy_kcal" %in% names(profile))
  expect_true("rec_low_fat_total_g" %in% names(profile))
  expect_true("perc_high_carbohydrates_g" %in% names(profile))
})

# --- TEST 2.2: PROFILE BATCH (CLEAN-UP & CONDENSATION) ---
test_that("process_intake_profiles condenses single targets and deletes empty recs", {
  
  batch_dir <- system.file("testdata", "intake_profiles", package = "evalfinutTools")
  
  master_profiles <- suppressMessages(
    process_intake_profiles(folder_path = batch_dir, output_folder = "Profile_Test_Results")
  )
  
  # 1. Check Row Count
  expect_equal(nrow(master_profiles), 2)
  
  # 2. Check Condensation Logic 
  # Vitamin C usually has identical low/high, so it should be condensed to 'rec_vitamin_c_mg'
  # instead of 'rec_low_vitamin_c_mg'
  if ("intake_vitamin_c_mg" %in% names(master_profiles)) {
    expect_true("rec_vitamin_c_mg" %in% names(master_profiles))
    expect_false("rec_low_vitamin_c_mg" %in% names(master_profiles))
  }
  
  # 3. Check Range Preservation
  # Fat total usually has a range (20-35%), so it should NOT be condensed
  expect_true("rec_low_fat_total_g" %in% names(master_profiles))
  expect_true("rec_high_fat_total_g" %in% names(master_profiles))
  
  # Clean up
  if (dir.exists("Profile_Test_Results")) unlink("Profile_Test_Results", recursive = TRUE)
})