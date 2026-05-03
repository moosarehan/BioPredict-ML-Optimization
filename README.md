# BioPredict-ML-Optimization 🧬

[![R Version](https://img.shields.io/badge/R-4.3.1-blue.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Optimized-success.svg)](#)

An advanced optimization and machine learning enhancement of the statistical methodologies presented in the research paper: **"Using logistic regression and the Cochran–Mantel–Haenszel test to analyze categorical data in biological experiments"**.

---

## 📄 Original Research Context

*   **Paper Title:** Using logistic regression and the Cochran–Mantel–Haenszel test to analyze categorical data in biological experiments
*   **Authors:** Rebecca Androwski*, Tatiana Popovitchenko*, Sho Ogino, Joelle Smart, Guoqiang Wang, Christopher Rongo, Monica Driscoll, Jason Roy (*co-first)
*   **Core Focus:** Providing a robust framework for analyzing categorical biological data (specifically exopher extrusion) using traditional statistical tests.

---

## ⚠️ The Problem: Statistical Limitations in Rare Event Detection

While the original paper provides a solid foundation using Logistic Regression (LR) and CMH tests, biological datasets—particularly those involving rare phenomena like exopher extrusion—often suffer from:
1.  **Extreme Class Imbalance:** Exophers are rare events, meaning "Negative" cases far outnumber "Positive" cases, causing standard LR to be biased toward the majority class.
2.  **Low Recall/Sensitivity:** Traditional models may fail to identify true positive exopher events, which are the most biologically significant.
3.  **Overfitting Risks:** Without rigorous cross-validation, models might capture noise rather than biological signal.

---

## 🚀 The 3 Key Improvisations

We introduced an **Improvised ML Pipeline (RF + SMOTE + CV)** to augment the original statistical analysis:

1.  **SMOTE (Synthetic Minority Over-sampling Technique):** 
    *   *Purpose:* To balance the dataset by synthetically generating minority class samples. 
    *   *Impact:* Significantly increased the model's sensitivity to rare biological events.
2.  **Random Forest (RF) Integration:**
    *   *Purpose:* Transitioned from linear models to an ensemble-based non-linear approach.
    *   *Impact:* Better captured complex, non-linear interactions between variables (e.g., aging vs. mating effects).
3.  **K-Fold Cross-Validation:**
    *   *Purpose:* Replaced single-split testing with robust 5/10-fold cross-validation.
    *   *Impact:* Ensured results are generalizable and not artifacts of a specific data split.

---

## 📊 Visualizations & Plots Produced

The following high-resolution visualizations were generated to compare the **Baseline** (Original Paper) vs. **Improvised** (Optimized) results:

*   **`improvised_ml_results.svg`**: Comparative metrics showing Accuracy, Precision, Recall, and F1-Score.
*   **`improvisation_recall_improvement.svg`**: A specialized plot highlighting the dramatic jump in Recall achieved via SMOTE.
*   **`master_performance_comparison.svg`**: A high-level overview comparing the AUC-ROC of traditional LR vs. Optimized RF.
*   **`improvised_predictive_probabilities.svg`**: Distribution plots showing how the model distinguishes between experimental groups.
*   **Experiment Specifics**:
    *   `exopher_frequency_plot.svg` (Baseline frequency distribution)
    *   `anoxiarecovery.svg` (Recovery rates in anoxia models)
    *   `exophermatingeffect.svg` (Impact of mating status on extrusion)

---

## 📂 Codebase Structure

```text
BioPredict-ML-Optimization/
├── Logistic_Regression_Aging/         # Aging Experiment Analysis
│   ├── input/                         # Raw datasets for aging
│   │   ├── Male_Supplement_Aged_Control_AD2_5.xlsx
│   │   ├── aged_exopher_final.xlsx
│   │   └── aged_exopher_final - Copy.xlsx
│   ├── Formatting_Input_Aging.R        # Preprocessing script
│   ├── Logistic_Regression_Aging.R      # ML Optimized analysis
│   ├── Logistic_Regression_Aging.Rproj # RStudio Project
│   ├── [Annotated_PDFs].pdf           # Detailed code/output walkthroughs
│   └── [Visualizations].svg           # Comparative performance plots
│
├── Logistic_Regression_Anoxia/        # Anoxia Experiment Analysis
│   ├── Input/                         # Recovery datasets
│   │   └── data_final.xlsx
│   ├── anoxia_20241218.R              # ML Optimized analysis
│   ├── Logistic_Regression_Anoxia.Rproj
│   ├── Anoxia_Code_Tutorial_Annotated.pdf
│   └── [Visualizations].svg           # Performance metrics & SVG plots
│
├── Logistic_Regression_Fertility/     # Fertility Experiment Analysis
│   ├── input/                         # Mating status datasets
│   │   ├── Male_Supplement_AD5.xlsx
│   │   └── fertility_exopher_final.xlsx
│   ├── Formatting_Input_Fertility.R   # Preprocessing script
│   ├── Logistic_Regression_Fertility.R # ML Optimized analysis
│   ├── Logistic_Regression_Fertility.Rproj
│   ├── [Annotated_PDFs].pdf           # Detailed walkthroughs
│   └── [Visualizations].svg           # Predictive Probabilities
│
├── README.md                          # Comprehensive Documentation
└── README.txt                         # Original Paper Documentation
```

---

## 🧬 Data Description

Exopher data included in this study is published in the following study:
> **Wang, G., Guasp, R. J., Salam, S., Chuang, E., Morera, A., Smart, A. J., ... & Driscoll, M. (2024).** Mechanical force of uterine occupation enables large vesicle extrusion from proteostressed maternal neurons. *eLife*, 13, RP95443.

### Raw Data Input Files
*   `Male_Supplement_Aged_Control_AD2_5.xlsx`
*   `Male_Supplement_AD5.xlsx`

### Reformatted Input Files
*   `aged_exopher_final.xlsx`
*   `fertility_exopher_final.xlsx`

---

## 🛠️ Requirements & Libraries

*   **R Version:** 4.3.1
*   **Core:** `dplyr`, `readxl`, `writexl`, `tidyr`, `stats`
*   **Visualization:** `ggplot2`, `ggthemes`, `ggthemeAssist`, `ggsignif`, `ggpubr`, `ggprism`
*   **Optimizations:** `randomForest`, `caret`, `smotefamily` (SMOTE implementation)

---

## ✉️ Contact Information

For any questions or clarifications, please contact the corresponding author:

*   **Name:** Rebecca Androwski
*   **Email:** rebecca.androwski@rutgers.edu
*   **Institution:** Rutgers University

---
*Developed for BioPredict-ML-Optimization. Visuals generated via R and SVG optimization.*
