# evalfinutTools: Dietary Assessment, NOVA Classification & Nutritional Adequacy

**evalfinutTools** is an R package designed to bridge the gap between raw dietary recall data and clinical nutritional analysis. It provides two distinct pipelines to handle both the granular food-item level (NOVA/Meals) and the clinical summary level (PRI Adequacy).

## Key Features

-   **Dual-Pipeline Architecture**: Separate workflows for raw "Metabol" food logs and EvalFinut "Mean Intake Profile" summaries.

-   **Automated NOVA Classification**: Map raw food records to NOVA groups 1–4 to identify ultra-processed food (UPF) contribution.

-   **Clinical Adequacy (PRI/RDA)**: Smart parsing of recommendation ranges (e.g., 20-35% Energy) and automatic calculation of % Adequacy.

-   **Self-Documenting Output**: Every exported dataset includes a generated **Codebook** and **README** to ensure reproducibility.

-   **Harmonized English Naming**: All Spanish-source data is automatically translated and standardized with unit-suffixed headers (e.g., `intake_protein_g`, `intake_energy_kcal`).

## Installation

```{r}
# Install from GitHub
remotes::install_github("PROFITH/evalfinutTools") 
```

## 1. Recall Pipeline: Raw Data & NOVA

Use this workflow when you have raw 24-hour recall files (Metabol format) and want to analyze food processing levels (NOVA) and meal distribution.

### Single File Analysis

```{r eval=FALSE}
library(evalfinutTools)  
# Import and analyze 
raw_data <- import_recall_data("data/metabol_file.xls") 
stats <- analyze_recalls(raw_data$D, food_db = bedca_db)  
# Access UPF contribution at Dinner 
print(stats$intake_energy_kcal) 
print(stats$cena_nova4_perc)  
```

### Batch Processing

Analyze an entire study folder. It produces a `Results Package` including a master CSV, a Codebook, and a README.

```{r eval=FALSE}
process_recall_24h(folder_path = "raw_recalls/", 
                   output_folder = "Study_NOVA_Results") 
```

## 2. Profile Pipeline: Nutritional Adequacy

Use this workflow when you have EvalFinut "Mean Intake Profiles" and want to assess if participants met their nutritional requirements (PRI/RDA).

### Batch Processing

This pipeline handles the complex string parsing of EvalFinut summaries, resolves recommendation ranges into Low/High bounds, and calculates % Adequacy.

```{r eval=FALSE}
process_intake_profiles(folder_path = "summary_profiles/", 
                        output_folder = "Study_Adequacy_Results") 
```

## Data Standardization

Both pipelines are harmonized to use a consistent naming convention, making it easy to merge results from both workflows.

### Column Naming Convention: `[Prefix]_[Nutrient]_[Unit]`

|  |  |
|------------------------------------|------------------------------------|
| **Prefix** | **Meaning** |
| **`total_daily_`** | The actual amount consumed (Grand Mean of daily totals). |
| **`rec_`** | The recommended target (single target or condensed range). |
| **`rec_low/high_`** | The bounds of a recommended range (common for Fats/Carbs). |
| **`perc_`** | The percentage of adequacy (Intake / Recommendation \* 100). |

### Common English Terms & Units

The package automatically handles Spanish-to-English translation and unit suffixes:

-   **Energy**: `_kcal`

-   **Macronutrients**: `_g` (Protein, Fat, Carbs, Fiber, etc.)

-   **Minerals/Vitamins**: `_mg` (Calcium, Iron, Vit C, etc.)

-   **Micronutrients**: `_ug` (Folate, Vit D, Vit B12, etc.)

## Output Structure

When using the **Batch Processing** functions, the package creates a standardized folder:

```         
Study_Results/ 
 ├── analysis_data.csv    # Master dataset (Regression-ready)
 ├── codebook.csv         # English dictionary of all variables and units
 └── README.txt           # Audit log with software versions and methodology 
```

## Configuration

### Standardized Meals

The tool enforces five standard meal slots for longitudinal consistency: *Desayuno, Media mañana, Almuerzo, Merienda,* and *Cena*.

### Internal Database

The package includes an internal `food_db` based on **BEDCA**, extended with specific items and pre-assigned **NOVA 1–4** classifications.

## License

**AGPL-3** \| Developed by the **PROFITH Research Group**.
