library(testthat)
library(dplyr)
library(readr)
library(purrr)
library(evalfinutTools)

# --- TEST 1: IMPORT ---

test_that("import_recall_data reads Metabol CSV format correctly", {
  
  file_path <- system.file("testdata", "metabol1.xls", package = "evalfinutTools")
  
  # Run Import
  result <- import_recall_data(file_path)
  
  # Check Structure
  expect_type(result, "list")
  expect_named(result, c("H", "D"))
  
  # Check Metadata (Header parsing)
  expect_equal(as.character(result$H$code), "Metabol")
  expect_equal(result$H$age, NA)
  
  # Check Data Cleaning
  df <- result$D
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 40)
  expect_equal(df$meal[1], "Desayuno")
  expect_equal(df$serving_g[1], 200)
  expect_type(df$food_id, "double")
})

# --- TEST 2: ANALYSIS (CALCULATIONS) ---

test_that("analyze_diet computes correct energy and imputes zeros", {
  
  # Load the data we just tested
  file_path <- system.file("testdata", "metabol2.xlsx", package = "evalfinutTools")
  raw_data <- import_recall_data(file_path)$D
  
  # Run Analysis
  stats <- analyze_diet(raw_data)
  
  # --- VALIDATION ---
  
  # 1. Output Format
  expect_s3_class(stats, "data.frame")
  expect_equal(nrow(stats), 1) # Should be 1 flattened row
  
  # 2. Math Check
  expect_equal(stats$total_daily_kcal, 1646.5, tolerance = 0.01)
  
  # 3. Check Imputation (Missing Meals)
  expect_true("desayuno_kcal" %in% names(stats))
  expect_equal(stats$desayuno_kcal, 201.8)
  
  # 4. Check NOVA columns
  expect_true("nova2_kcal" %in% names(stats))
  expect_gt(stats$nova2_kcal, 0) 
})

# --- TEST 3: BATCH PROCESSING ---

test_that("process_dataset iterates through folder and merges results", {
  
  # Point to the 'batch' subfolder inside testfiles
  batch_dir <- system.file("testdata", package = "evalfinutTools")
  
  # Run Batch (Output CSV disabled for test)
  # Suppress messages to keep test console clean
  master_df <- suppressMessages(process_dataset(batch_dir, output_csv = NULL))
  
  # --- VALIDATION ---
  
  # 1. Should have 2 rows (p1.csv and p2.csv)
  expect_equal(nrow(master_df), 2)
  
  # 2. Check Identification
  # The 'source_file' column should contain the filenames
  expect_true(any(grepl("metabol1.xls", master_df$source_file)))
  expect_true(any(grepl("metabol2.xlsx", master_df$source_file)))
  
  # 3. Check Metadata Merging
  # The columns from the header (H) should be present
  expect_true("sex" %in% names(master_df))
  
  # 4. Check consistency
  # Both rows should have 'total_daily_kcal'
  expect_false(any(is.na(master_df$total_daily_kcal)))
})