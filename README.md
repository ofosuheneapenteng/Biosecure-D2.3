#  Biosecure Project – Hotspot Mapping Pipeline for Avian Influenza (AIV) and African Swine Fever (ASF)

This repository contains the full R-based pipeline for mapping and analyzing Avian Influenza (AIV) risk across NUTS2 regions in the EU. It integrates biosecurity metrics, environmental drivers, and disease case data to estimate regional risk scores and visualize hotspots. 

**Note the flow or process in the AIV is the same African Swine Fever (ASF) code**
---

## Project Structure

- **Data Sources**:
  - Biocheck data (2023 & 2024) for poultry types:
    - Free-range layers
    - Free-range broilers
    - Broilers
    - Laying hens
  - Bird density and land cover data
  - AIV poultry case counts
  - NUTS2 shapefiles and metadata

- **Main Outputs**:
  - Cleaned and merged datasets
  - Risk scores and disease introduction probabilities
  - Spatial visualizations of risk and scenario comparisons
  - Statistical modeling results (Negative Binomial regression)

---
## FAIR Data Principles
This repository follows the **FAIR data principles** to ensure that all resources are **Findable, Accessible, Interoperable, and Reusable**. The open-source R code is fully documented and version-controlled, and the harmonised datasets are structured to support reproducibility and integration with other tools and workflows. By aligning with FAIR standards, this project promotes transparency, encourages collaboration, and supports the broader research community in advancing spatial risk mapping for disease surveillance.

---
##  Pipeline Overview

### 1. Data Cleaning & Aggregation
- Reads and processes Biocheck Excel sheets
- Aggregates mean scores by region and question ID
- Merges 2023 and 2024 scores for each poultry type

### 2. Driver Integration
- Combines bird density and land cover with Biocheck scores
- Joins AIV case data to form the final modeling dataset

### 3. Modeling
- Fits a Negative Binomial model using `glmmTMB`
- Estimates dispersion parameter (theta) via profile likelihood
- Computes risk scores and disease introduction probabilities

### 4. Visualization
- Maps risk scores and disease introduction using `ggplot2` and `sf`
- Compares baseline vs. biosecurity improvement/decrease scenarios
- Highlights top 15 regions with disease introduction

### 5. Spatial Analysis
- Computes local spatial autocorrelation (Moran’s I)
- Generates high-resolution maps and side tables

---

## Key Figures

- `AIV_Figures/asf_risk_score_map.png`: Risk score map
- `AIV_Figures/disease_risk_map_with_side_legend_tight.png`: Disease introduction map with top regions
- `AIV_Figures/Risk Score After 20% Biosecurity Increase_map.png`: Scenario comparison (boost)
- `AIV_Figures/Risk Score After 20% Biosecurity Decrease_map.png`: Scenario comparison (decline)

---

##  Saved RDS Files

- `Free_range_layers_Mean_Value.rds`
- `Free_range_broilers_Mean_Value.rds`
- `Broilers_Mean_Value.rds`
- `Laying_hens_Mean_Value.rds`
- `nuts2_drivers_biocheck_AIV.rds`
- `nuts2_risk_sf_aiv.rds`
- `AIV_Poultry_Birds.rds`
- `bird_density.rds`
- `nuts2_land_cover.rds`

---

## Dependencies

Make sure the following R packages are installed:


library(sf)
library(tidyverse)
library(glmmTMB)
library(readxl)
library(giscoR)
library(eurostat)
library(ggplot2)
library(gganimate)
library(gstat)
library(sp)
library(terra)
library(raster)
library(lattice)
library(httr)
library(utils)
library(cowplot)
library(stringr)
library(spdep)




 ## Author

This R code was developed and written by **Ofosuhene Okofrobour Apenteng**,for mapping biosecurity risks of African Swine Fever (ASF) and Avian Influenza Virus (AIV) across EU regions. Developed for the Biosecure project (EU Horizon 101083923).


