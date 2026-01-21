# evalfinutTools

A comprehensive R package for processing and analyzing dietary intake data exported from the [Evalfinut](https://www.evalfinut.com/) nutritional assessment software.

This package includes functions to:

- Format participant-level nutritional summaries
- Import and clean food intake records
- Classify foods by processing level using the **NOVA index**
- Compute energy intake by meal (e.g. breakfast, lunch, dinner, snacks) and NOVA group

---

## Installation

Install from local source or from GitHub (if published):

```r
# If using devtools
devtools::install_github("yourusername/evalfinutTools")

# Or from a local folder
devtools::install_local("path/to/evalfinutTools")
```

# Features
format_evalfinut_summary()
Reads .xls summary-by-person exports

Cleans and standardizes nutrient column names

Extracts:

Intake (ingesta)

Recommendations (recomendado)

Differences (diferencia, including ranges)

Outputs a single tidy row per participant

read_food_intake()
Reads detailed food intake data (multiple rows per participant)

Parses meal types and time of consumption

Prepares data for downstream NOVA classification

classify_nova()
Classifies each food item into NOVA groups (1–4)

Supports internal or external NOVA mapping tables

calculate_energy_by_meal_and_nova()
Aggregates energy intake by:

Meal: breakfast, lunch, dinner, snacks

NOVA group: unprocessed to ultra-processed

Outputs a summary per participant

Use Case
Designed for researchers conducting:

Nutritional epidemiology

Dietary quality analysis

Studies of food processing and health outcomes

📝 Example
r
Copiar
Editar
library(evalfinutTools)

# Format summary file
summary_df <- format_evalfinut_summary("data/summary_participant1.xls")

# Load detailed intake
intake_df <- read_food_intake("data/intake_participant1.xlsx")

# Classify foods
intake_df <- classify_nova(intake_df, nova_table = "data/nova_lookup.csv")

# Calculate energy intake
energy_summary <- calculate_energy_by_meal_and_nova(intake_df)
📂 Planned Structure
kotlin
Copiar
Editar
evalfinutTools/
├── R/
│   ├── format_summary.R
│   ├── read_food_intake.R
│   ├── classify_nova.R
│   └── calc_energy_intake.R
├── data/
│   └── nova_lookup.csv
├── man/
├── DESCRIPTION
├── NAMESPACE
├── README.md
└── tests/
📄 License
MIT License. See LICENSE file for details.

📬 Contact
For issues, please open an Issue or contact your.email@example.com.

yaml
Copiar
Editar

---

Let me know if you’d like help:

- Structuring your `R/` source files
- Writing stubs or templates for the other functions (`read_food_intake()`, `classify_nova()`, etc.)
- Creating a sample `nova_lookup.csv` mapping table
- Setting up unit tests or vignettes

We can gradually build this into a well-structured and publishable package.



alimentos completos.csv
-----
base de datos BEDCA + etiquetas nutricionales de alimentos introducidas a mano

"# evalfinutTools" 
