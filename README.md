# STATS452_Final_Project

Final project for **STAT452 – Applied Statistics for Engineers and Scientists II** at **VNUHCM-University of Science**.

This repository collects the project code, datasets, rendered reports, and final written report for three applied statistical analyses in R:

- movie gross revenue prediction
- GPU price and performance analysis
- heart disease classification and severity modeling

## Project Overview

The project is organized into three main analytical tasks:

1. **Activity 1 – CSM**: predictive modeling for movie gross revenue using the `CSM.xlsx` dataset.
2. **Activity 1 – GPU**: exploratory and statistical analysis of GPU specifications, release timing, and price/performance behavior.
3. **Activity 2 – Heart Disease**: binary and ordinal modeling for heart disease diagnosis and severity.

Each analysis includes:

- an R script in `Code/`
- a rendered HTML report in `Code/`
- supporting datasets in `Dataset/`

The repository also includes the final compiled report in `Final_report.pdf`.

## Repository Structure

```text
STATS452_Final_Project/
├── Code/
│   ├── Activity1_CSM.R
│   ├── Activity1_CSM.html
│   ├── Activity1_GPU.r
│   ├── Activity1_GPU.html
│   ├── Activity2.r
│   └── Activity2.html
├── Dataset/
│   ├── All_GPUs.csv
│   ├── CSM.xlsx
│   └── heart_disease_uci.csv
├── Final_report.pdf
├── LICENSE
└── README.md
```

## Datasets

### 1. `Dataset/CSM.xlsx`

- Input for the movie gross revenue analysis.
- Used for data preprocessing, missing-data investigation, imputation, feature engineering, and regression modeling.

### 2. `Dataset/All_GPUs.csv`

- Input for the GPU analysis.
- Contains **3,406 rows** of GPU records.
- Includes fields related to manufacturer, architecture, memory, clock rates, pixel rate, release timing, and release price.

### 3. `Dataset/heart_disease_uci.csv`

- Input for the heart disease analysis.
- Contains **920 rows** of patient observations.
- Includes demographic and clinical variables such as `age`, `sex`, `cp`, `trestbps`, `chol`, `thalch`, `oldpeak`, `ca`, `thal`, and target variable `num`.

## Analysis Summary

### Activity 1: Movie Gross Revenue Prediction

Files:

- `Code/Activity1_CSM.R`
- `Code/Activity1_CSM.html`

Main workflow:

- data cleaning and preprocessing
- missing-data analysis and missingness diagnostics
- imputation using `missForest`
- exploratory data analysis
- train/validation split
- multiple linear regression and model selection using backward procedures and subset selection

Reported validation results from `Code/Activity1_CSM.html`:

- **MAE**: `32,841,452`
- **RMSE**: `50,039,403`
- **R²**: `0.6118674`

This analysis focuses on identifying useful predictors of movie gross revenue while addressing nontrivial missing-data patterns.

### Activity 1: GPU Price and Performance Analysis

Files:

- `Code/Activity1_GPU.r`
- `Code/Activity1_GPU.html`

Main workflow:

- parsing and cleaning hardware specification fields
- converting text-based numeric columns into usable measurements
- deriving time variables such as release year, month, and quarter
- descriptive visualization of pricing and performance patterns
- missing-data analysis and discussion of imputation limitations
- comparison of performance-to-price behavior over time

Key result from `Code/Activity1_GPU.html`:

- quarterly performance-to-price differences were statistically significant by ANOVA with **`p < 2e-16`**

This analysis emphasizes exploratory insight, temporal trends, and caution around missing `Release_Price` information.

### Activity 2: Heart Disease Analysis

Files:

- `Code/Activity2.r`
- `Code/Activity2.html`

Main workflow:

- preprocessing and factor conversion
- missing-data imputation using `missForest`
- creation of a binary disease indicator
- univariate and multivariable logistic regression
- transformation checks using Box-Tidwell ideas
- AIC/BIC-based variable selection
- ordinal logistic regression for disease severity among diseased patients

Selected reported results from `Code/Activity2.html`:

- **Binary classification accuracy**: `0.8804`
- **Binary ROC AUC**: `0.9567568`
- **Ordinal accuracy (stepwise AIC)**: `0.5090909`
- **Ordinal accuracy (stepwise BIC)**: `0.5636364`

This analysis combines binary classification performance with an additional ordinal modeling step for severity prediction.

## Outputs

- `Code/Activity1_CSM.html`: rendered report for the movie revenue analysis
- `Code/Activity1_GPU.html`: rendered report for the GPU analysis
- `Code/Activity2.html`: rendered report for the heart disease analysis
- `Final_report.pdf`: compiled final report for the course project

## Tools and Packages

The R scripts use common data science and statistical packages, including combinations of:

- `ggplot2`
- `dplyr`
- `caret`
- `missForest`
- `MASS`
- `lmtest`
- `car`
- `GGally`
- `corrplot`
- `xgboost`
- `brglm`
- `openxlsx`

Package usage differs by activity, so install the packages needed for the script you want to run.

## Reproducibility Notes

There are a few practical caveats if you want to rerun the scripts directly:

- some scripts contain hardcoded `setwd(...)` or machine-specific path logic
- at least one script uses `datasets/...` style paths, while this repository stores data under `Dataset/`
- you may need to update file paths before running the code on another machine

Recommended approach:

1. Open the project in RStudio or VS Code.
2. Adjust working-directory and input-data paths to match your local clone.
3. Install required R packages.
4. Run the relevant script in `Code/`.

## How to Use This Repository

- Read `README.md` for the project map and summary.
- Open the `.R` files in `Code/` to inspect the full analysis workflow.
- Open the `.html` files in `Code/` to review rendered results and figures.
- Read `Final_report.pdf` for the final write-up.

## Course Context

This repository is an academic project submission for STAT452 and demonstrates applied statistical workflows involving:

- preprocessing and exploratory analysis
- missing-data handling
- regression and classification modeling
- model selection and evaluation
- interpretation of real-world datasets
