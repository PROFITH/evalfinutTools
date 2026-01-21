# evalfinutTools: Dietary Assessment & NOVA Classification

**evalfinutTools** is an R package designed to streamline the analysis of 24-hour dietary recalls. It automates the calculation of energy intake and classifies food items according to the **NOVA classification system** (Ultra-Processed Foods), specifically tailored for "Metabol" format exports and BEDCA-based food databases.

## Features

-   **Automated Import**: Parses complex Excel/CSV recall files with specific "Metabol" headers and metadata.
-   **NOVA Classification**: Automatically groups foods into NOVA 1 (Unprocessed) through NOVA 4 (Ultra-Processed) using an internal reference database.
-   **Robust Imputation**: Correctly handles missing meals (e.g., if a participant skips dinner, it records 0 kcal rather than missing data) to ensure statistical consistency.
-   **Batch Processing**: Analyze hundreds of participant files in a single command with a built-in progress bar.
-   **Regression-Ready Output**: Returns a "wide" format dataset (one row per participant) ideal for statistical modeling.

------------------------------------------------------------------------

## Installation

You can install the package from GitHub:

``` r
library(remotes)
remotes::install_github("PROFITH/evalfinutTools")
```

------------------------------------------------------------------------

## Usage

### 1. Processing a Single File

If you want to check a specific participant's data:

``` r
library(evalfinutTools)

# 1. Import the raw file
# Separates metadata (Age, Sex, etc.) from the dietary records
participant_data <- import_recall_data("raw_data/Metabol001.xls")

# 2. Analyze the diet
# Calculates sums, means, and imputes zeros for skipped meals
stats <- analyze_diet(participant_data$D)

# 3. View results
print(stats$total_daily_kcal)
print(stats$cena_nova4_perc) # % of Dinner calories from UPF
```

### 2. Batch Processing (Recommended Workflow)

To analyze an entire study folder containing multiple `.xls` or `.xlsx` files:

``` r
library(evalfinutTools)

# 1. Import the raw file
# Separates metadata (Age, Sex, etc.) from the dietary records
participant_data <- import_recall_data("raw_data/Metabol001.xls")

# 2. Analyze the diet
# Calculates sums, means, and imputes zeros for skipped meals
stats <- analyze_diet(participant_data$D)

# 3. View results
print(stats$total_daily_kcal)
print(stats$cena_nova4_perc) # % of Dinner calories from UPF
```

------------------------------------------------------------------------

## Output Data Dictionary

The package produces a "wide" format dataset where each row represents one participant.

|  |  |
|----------------------------|--------------------------------------------|
| **Column Name** | **Description** |
| `source_file` | Name of the original input file. |
| `code` | Participant ID (extracted from file header). |
| `age`, `sex` | Demographics (extracted from file header). |
| `total_daily_kcal` | Mean daily energy intake (kcal). |
| `[meal]_kcal` | Absolute energy for a specific meal (e.g., `almuerzo_kcal`). |
| `nova[1-4]_kcal` | Energy from specific NOVA groups (e.g., `nova4_kcal` = UPF). |
| `[meal]_nova[1-4]_perc` | Percentage of that meal's energy coming from that NOVA group. |

------------------------------------------------------------------------

## Configuration

### Standard Meals

To ensure dataset consistency across participants, the package enforces that the following meal columns always exist (filled with 0 if missing):

-   *Desayuno (Breakfast)*

-   *Media mañana (Mid-morning snack)*

-   *Almuerzo (Lunch)*

-   *Merienda (Afternoon snack)*

-   *Cena (Dinner)*

### Internal Database

The package includes an internal food database (`food_db`) derived from BEDCA plus some manually introduced foods. This database includes:

-   `food_id`: Unique link to recall data.

-   `nova_group`: The pre-assigned NOVA class (1-4).

-   `kcal_per_100g`: Energy density.

------------------------------------------------------------------------

## License

AGPL-3
