## 📖 Master's Thesis
My full master's thesis, titled **"The associations between soil properties and aphid abundance in soybean agroecosystem"**, is officially published and available for download here:
* [NTU Theses and Dissertations Repository (臺灣大學碩博士論文典藏系統)](http://hdl.handle.net/2462/NTU202600178)
* **Permanent DOI**: [https://doi.org/10.6342/NTU202600178](https://doi.org/10.6342/NTU202600178)

# 📉 Independent & Seasonal Partial Regression Models (WAP/MAP Residual Pipeline)

This repository contains the advanced biostatistical analysis workflow and `ggplot2` pipeline used to construct **Partial Regression /偏相關圖 (Residual Plots)** for my Master's Thesis. 

The primary framework removes temporal confounding factors (Time-since-planting variables) from both the biological response and soil environmental variables to evaluate true underlying ecological links.

---

## 📌 Biostatistical Methodology Overview

In agroecosystem scouting, variables like **Aphid Abundance** and **Soil Electrical Conductivity (EC)** or **Moisture** heavily covariate with plant phenology over time. Direct correlation tests can yield misleading coefficients due to shared seasonal patterns. 

To isolate the true effect size, this script implements a **two-stage residual control framework**:
1. **Confounder Modeling:** Constructs baseline linear models (`lm`) predicting both the biological response and soil property using temporal variables (**WAP: Weeks After Planting** or **MAP: Months After Planting**) as independent predictors.
2. **Residual Extraction & Alignment:** Extracts the calculated unexplained variance (residuals):
   * $\text{Aphid Residuals} = Y_{\text{Aphid}} - \hat{Y}_{(\text{WAP/MAP})}$
   * $\text{Soil Residuals} = X_{\text{Soil}} - \hat{X}_{(\text{WAP/MAP})}$
3. **Partial Correlation:** Maps the aligned residuals via Pearson’s correlation metrics to generate true partial regression slopes free from crop growth phase biases.

---

## 📊 Figure Structure & Subplot Architecture

The script processes raw microclimate and pest vectors to generate comparative matrix plots split across distinct structural layers:

### 🔹 Core Modeling Panels
* **Raw Associations (No Control):** Scatter plots mapping unadjusted $log_{10}(\text{Aphids} + 1)/\text{m}^2$ values against daily soil factors, capturing the raw macroscopic trend.
* **WAP Trajectory Control:** Plots correlation distributions using residuals filtered through **Weeks After Planting**, isolating intra-week fluctuations.
* **MAP Trajectory Control:** Plots correlation distributions using residuals filtered through **Months After Planting**, identifying broader multi-week trends.

### 🔹 Seasonal Stratification & Plot Settings
* **Color Mapping:** Uses dedicated, thesis-consistent hex color palettes to distinguish crop seasons:
  * **2024 Spring ($n = 390$):** Highlighted in Forest Green (`#006600`).
  * **2024 Fall ($n = 432$):** Highlighted in Earth Brown (`#867052`).
* **Visual Annotations:** Leverages `ggplot2::annotate` to automatically print calculated Pearson correlation coefficients ($r$) and statistical alpha benchmarks (*p*-values) color-coded to each respective crop cycle.
* **Trend Estimation:** Layers standardized linear regression fits (`geom_smooth(method = "lm")`) across pooled datasets and seasonal data columns.

---

## 🛠️ Required Libraries

```r
library(tidyverse)   # Data manipulation & ggplot2 architecture
library(readxl)      # Ingestion of multi-sheet data matrices
library(car)         # Diagnostic checks for linear models
