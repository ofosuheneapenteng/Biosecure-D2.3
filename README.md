
#  Biosecure Risk Mapping Pipelines

This repository contains reproducible R scripts for analyzing biosecurity risks of two major transboundary animal diseases:

- **African Swine Fever (ASF)**
- **Avian Influenza Virus (AIV)**

Developed as part of the **Biosecure project (EU Horizon 101083923)**, these pipelines integrate heterogeneous interdisciplinary datasets to generate spatial risk maps at the NUTS2 level across the European Union.



## Features

- Integration of epidemiological, biosecurity, environmental, and demographic data
- Risk modeling using Negative Binomial regression and profile likelihood
- Scenario simulation for biosecurity improvement
- Clustering analysis and spatial autocorrelation (Moran’s I)
- High-resolution geospatial visualizations using ggplot2 and sf



##  Repository Structure


biosecure-risk-maps/
├── README.md
├── LICENSE
├── scripts/
│   ├── ASF_pipeline.R
│   └── AIV_pipeline.R
├── data/
│   ├── example_inputs/
│   └── processed_rds/
├── figures/
│   ├── ASF/
│   └── AIV/
├── results/
│   ├── ASF/
│  └── AIV/



## Technologies Used

- **R packages**: `sf`, `tidyverse`, `ggplot2`, `glmmTMB`, `MASS`, `car`, `cluster`, `spdep`, `readxl`, `terra`, `giscoR`, `eurostat`, `gganimate`, `gstat`, `cowplot`, `ggrepel`, and more
- **Data formats**: `.xlsx`, `.rds`, `.csv`, `.shp`
- **Visualization**: Static and animated maps saved as `.png`



##  Outputs

- ASF and AIV risk scores (scaled 0–100)
- Probability of disease introduction
- Cluster maps and profiles
- Scenario comparison maps (e.g., 60% biosecurity improvement)
- Disease introduction maps with labeled regions
- Probability curves with confidence intervals

 ## Author

This R code was developed and written by **Ofosuhene Okofrobour Apenteng**,for mapping biosecurity risks of African Swine Fever (ASF) and Avian Influenza Virus (AIV) across EU regions.  
Developed for the Biosecure project (EU Horizon 101083923).


