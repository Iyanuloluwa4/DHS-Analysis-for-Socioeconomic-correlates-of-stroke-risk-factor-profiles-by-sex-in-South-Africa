# DHS-Analysis-for-Socioeconomic-correlates-of-stroke-risk-factor-profiles-by-sex-in-South-Africa
# South Africa DHS Stroke Risk Factor Analysis

## Overview

This repository contains the complete R workflow used to prepare, analyse, and model nationally representative data from the 2016 South Africa Demographic and Health Survey (DHS). The project investigates sex-specific socioeconomic and behavioural correlates of major modifiable stroke risk factors using complex survey methods.

The analyses were conducted as part of a Master of Public Health (International) dissertation at the University of Leeds.

---

## Research Question

**What are the socioeconomic correlates of major modifiable stroke risk factors among South African adults, and do these associations differ by sex?**

---

## Objectives

1. Estimate the weighted prevalence of hypertension, diabetes, obesity, and smoking among South African adults.

2. Examine associations between socioeconomic characteristics and each stroke risk factor using survey-weighted logistic regression.

3. Investigate whether socioeconomic characteristics modify the relationship between sex and each stroke risk factor through interaction analyses.

---

## Data Source

South Africa Demographic and Health Survey (SADHS) 2016

The analysis uses the following DHS datasets:

* Household Recode (HR)
* Individual Recode (IR)
* Men's Recode (MR)

These datasets are **not included** in this repository.

Researchers can request access through the DHS Program.

---

## Outcomes

The following stroke risk factors were examined:

* Hypertension
* Diabetes
* Obesity
* Current smoking

Outcome definitions were based on internationally accepted clinical thresholds and DHS biomarker protocols.

---

## Key Features

* Complex survey design analysis
* Survey weighting
* Stratified cluster sampling
* Biomarker data processing
* Household-to-individual linkage
* Interaction modelling
* Sex-stratified analyses
* Survey-weighted logistic regression

---

## Repository Structure

```
Stroke-DHS-SouthAfrica/

├── README.md
├── R/
│   ├── 01_data_preparation.R
│   ├── 02_outcome_creation.R
│   ├── 03_prevalence_analysis.R
│   ├── 04_regression_models.R
│   └── 05_interaction_models.R
│
├── data/
│   └── README.txt
│
└── output/
```

---

## Required R Packages

```r
dplyr
tidyr
haven
survey
labelled
```

---

## Analytical Workflow

```
Raw DHS datasets
        │
        ▼
Data cleaning
        │
        ▼
Person-level biomarker linkage
        │
        ▼
Outcome derivation
(Hypertension, Diabetes,
Obesity, Smoking)
        │
        ▼
Survey weighting
        │
        ▼
Weighted prevalence estimates
        │
        ▼
Survey-weighted logistic regression
        │
        ▼
Interaction analyses
```

---

## Survey Design

The analyses account for the complex DHS sampling design by incorporating:

* Primary Sampling Units (PSU)
* Sampling strata
* Sampling weights

using the `survey` package in R.

---

## Reproducibility

To reproduce the analyses:

1. Obtain permission to download the South Africa DHS datasets from the DHS Program.
2. Place the datasets inside the `data/` folder.
3. Update the file paths in the R scripts if necessary.
4. Run the scripts sequentially.

---

## Citation

If you use or adapt this code, please cite:

Ojo IS. *Socioeconomic and healthcare access correlates of stroke risk factors by sex in South Africa: A Demographic and Health Survey–based analysis.* Master of Public Health Dissertation, University of Leeds.

---

## Disclaimer

This repository contains analysis code only.

The DHS datasets are the property of The DHS Program and cannot be redistributed. Users must obtain the datasets directly from the DHS Program before running the analyses.

---

## Author

**Dr. Iyanuloluwa Samuel Ojo**

Medical Doctor | Epidemiologist | Health Data Scientist

University of Leeds

GitHub: https://github.com/Iyanuloluwa4

LinkedIn: https://www.linkedin.com/in/oluwa-o-b2c1b1137/

---
