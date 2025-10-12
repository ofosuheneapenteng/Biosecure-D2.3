rm(list = objects() )
# ---- Load packages ----
library(sf)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(tmap)
library(readr)
library(httr)
library(utils)
library(gstat)
library(lattice)
library(readxl)
library(sp)
library(terra)
library(dplyr)
library(eurostat)
library(raster)
library(giscoR)
library(stringr)
library(eurostat)

setwd("~/Desktop/Biosecure_Project-Hotspot-Mapping_Final")

#----Biosecurity measurers-------
#--All birds based on countries-- 2023/2024 --------------
X2023_reg_Biocheck_EU_BIRDS_Free_range_layers <- read_excel("2023_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Free_range_layers')
X2023_reg_Biocheck_EU_BIRDS_Free_range_broilers <- read_excel("2023_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Free_range_broilers')
X2023_reg_Biocheck_EU_BIRDS_Broilers <- read_excel("2023_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Broilers')
X2023_reg_Biocheck_EU_BIRDS_Laying_hens <- read_excel("2023_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Laying_hens')
unique(X2023_reg_Biocheck_EU_BIRDS_Free_range_layers$Country)
unique(X2023_reg_Biocheck_EU_BIRDS_Free_range_broilers$Country)
unique(X2023_reg_Biocheck_EU_BIRDS_Broilers$Country)
unique(X2023_reg_Biocheck_EU_BIRDS_Laying_hens$Country)

Biocheck_Region_Mean_Free_range_layers_2023 <- X2023_reg_Biocheck_EU_BIRDS_Free_range_layers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

Biocheck_Region_Mean_Free_range_broilers_2023 <- X2023_reg_Biocheck_EU_BIRDS_Free_range_broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

Biocheck_Region_Mean_Broilers_2023 <- X2023_reg_Biocheck_EU_BIRDS_Broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

Biocheck_Region_Mean_Laying_hens_2023 <- X2023_reg_Biocheck_EU_BIRDS_Laying_hens %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M","N")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#---------------2024-----------------
X2024_reg_Biocheck_EU_BIRDS_Free_range_layers <- read_excel("2024_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Free_range_layers')
X2024_reg_Biocheck_EU_BIRDS_Free_range_broilers <- read_excel("2024_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Free_range_broilers')
X2024_reg_Biocheck_EU_BIRDS_Broilers <- read_excel("2024_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Broilers')
X2024_reg_Biocheck_EU_BIRDS_Laying_hens <- read_excel("2024_reg_Biocheck_EU_BIRDS.xlsx", sheet = 'Laying_hens')
unique(X2024_reg_Biocheck_EU_BIRDS_Free_range_layers$Country)
unique(X2024_reg_Biocheck_EU_BIRDS_Free_range_broilers$Country)
unique(X2024_reg_Biocheck_EU_BIRDS_Broilers$Country)
unique(X2024_reg_Biocheck_EU_BIRDS_Laying_hens$Country)

Biocheck_Region_Mean_Free_range_layers_2024 <- X2024_reg_Biocheck_EU_BIRDS_Free_range_layers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

Biocheck_Region_Mean_Free_range_broilers_2024 <- X2024_reg_Biocheck_EU_BIRDS_Free_range_broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

Biocheck_Region_Mean_Broilers_2024 <- X2024_reg_Biocheck_EU_BIRDS_Broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

Biocheck_Region_Mean_Laying_hens_2024 <- X2024_reg_Biocheck_EU_BIRDS_Laying_hens %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M","N")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  #dplyr::rename(ADMIN = Country) %>% 
  distinct() 

df1 = Biocheck_Region_Mean_Free_range_layers_2023
df2 = Biocheck_Region_Mean_Free_range_layers_2024

#Sum their value columns, treating missing values as 0.
# Full outer join using full_join
full_merged_Free_range_layers <- full_join(df1, df2, by = c("ID","NAME_LATN"), suffix = c("_df1", "_df2"))

# Replace NAs with 0 and sum the values
Free_range_layers_Mean_Value <- full_merged_Free_range_layers %>%
  mutate(
    Mean_score_Region_df1 = replace_na(Mean_score_Region_df1, 0),
    Mean_score_Region_df2 = replace_na(Mean_score_Region_df2, 0),
    Free_range_layers_Mean_value = Mean_score_Region_df1 + Mean_score_Region_df2
  ) %>%
  dplyr::select(ID, NAME_LATN, Free_range_layers_Mean_value)%>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

print(Free_range_layers_Mean_Value )
saveRDS(Free_range_layers_Mean_Value, "Free_range_layers_Mean_Value.rds")

#------Free_range_broilers-------------
df3 = Biocheck_Region_Mean_Free_range_broilers_2023
df4 = Biocheck_Region_Mean_Free_range_broilers_2024

#Sum their value columns, treating missing values as 0.
# Full outer join using full_join
full_merged_Free_range_broilers <- full_join(df3, df4, by = c("ID","NAME_LATN"), suffix = c("_df3", "_df4"))

# Replace NAs with 0 and sum the values
Free_range_broilers_Mean_Value <- full_merged_Free_range_broilers %>%
  mutate(
    Mean_score_Region_df3 = replace_na(Mean_score_Region_df3, 0),
    Mean_score_Region_df4 = replace_na(Mean_score_Region_df4, 0),
    Free_range_broilers_Mean_value = Mean_score_Region_df3 + Mean_score_Region_df4
  ) %>%
  dplyr::select(ID, NAME_LATN, Free_range_broilers_Mean_value)%>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

print(Free_range_broilers_Mean_Value )
saveRDS(Free_range_broilers_Mean_Value, "Free_range_broilers_Mean_Value.rds")

#------Broilers-------------
df5 = Biocheck_Region_Mean_Broilers_2023
df6 = Biocheck_Region_Mean_Broilers_2024

#Sum their value columns, treating missing values as 0.
# Full outer join using full_join
full_merged_Broilers <- full_join(df5, df6, by = c("ID","NAME_LATN"), suffix = c("_df5", "_df6"))

# Replace NAs with 0 and sum the values
Broilers_Mean_Value <- full_merged_Broilers %>%
  mutate(
    Mean_score_Region_df5 = replace_na(Mean_score_Region_df5, 0),
    Mean_score_Region_df6 = replace_na(Mean_score_Region_df6, 0),
    Broilers_Mean_value = Mean_score_Region_df5 + Mean_score_Region_df6
  ) %>%
  dplyr::select(ID, NAME_LATN, Broilers_Mean_value)%>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

print(Broilers_Mean_Value)
saveRDS(Broilers_Mean_Value, "Broilers_Mean_Value.rds")

#------Laying_hens-------------
df7 = Biocheck_Region_Mean_Laying_hens_2023
df8 = Biocheck_Region_Mean_Laying_hens_2024

#Sum their value columns, treating missing values as 0.
# Full outer join using full_join
full_merged_Laying_hens <- full_join(df7, df8, by = c("ID","NAME_LATN"), suffix = c("_df7", "_df8"))

# Replace NAs with 0 and sum the values
Laying_hens_Mean_Value <- full_merged_Laying_hens %>%
  mutate(
    Mean_score_Region_df3 = replace_na(Mean_score_Region_df7, 0),
    Mean_score_Region_df4 = replace_na(Mean_score_Region_df8, 0),
    Laying_hens_Mean_value = Mean_score_Region_df7 + Mean_score_Region_df8
  ) %>%
  dplyr::select(ID, NAME_LATN, Laying_hens_Mean_value)%>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

print(Laying_hens_Mean_Value)
saveRDS(Laying_hens_Mean_Value, "Laying_hens_Mean_Value.rds")


#--------------------Download the save drivers -------------------
df_bird <- st_drop_geometry(readRDS("bird_density.rds"))
df_cover <- st_drop_geometry(readRDS("nuts2_land_cover.rds"))

# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")
# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 = nuts2_sf


nuts2_combined <- nuts2 

#----------------START FROM HERE-----------------------
nuts2_drivers_aiv <- readRDS("nuts2_drivers_clean.rds")
View(nuts2_drivers_aiv)

#--------Remove pig density since we used in the ASF model
nuts2_drivers_aiv <- nuts2_drivers_aiv %>% dplyr::select(-pig_density)

# 1. Bird density data and drop geometry
df_bird <- st_drop_geometry(readRDS("bird_density.rds"))

# 2. Check land cover keys
if (!"NAME_LATN" %in% names(df_bird)) {
  stop("`NAME_LATN` not found in df_bird")
}

# 3. Fix nuts2_combined: rename if needed
if ("NAME_LATN.x" %in% names(nuts2)) {
  nuts2_combined <- nuts2 %>%
    rename(NAME_LATN = NAME_LATN.x)
}

if (!"NAME_LATN" %in% names(nuts2_combined)) {
  stop("`NAME_LATN` not found in nuts2_combined after renaming")
}

# 4. Join birds density data and add the land cover
nuts2_joined_aiv <- nuts2_combined %>%
  left_join(df_bird, by = "NAME_LATN")%>%
  left_join(df_cover, by = "NAME_LATN")

# 5. Remove duplicate columns with .x and .y suffixes created by joins
nuts2_clean_aiv <- nuts2_joined_aiv %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"))

# 6. Drop geometry (if any)
nuts2_clean_aiv <- nuts2_clean_aiv %>%
  sf::st_drop_geometry() %>%
  rename(bird_density="total_median_abundance")

# 7. Select only the final columns needed
nuts2_drivers_aiv <- nuts2_clean_aiv %>%
  dplyr::select(
    NUTS_ID, CNTR_CODE, NAME_LATN,
    bird_density, land_cover
  )

# 8. Save final cleaned data
saveRDS(nuts2_drivers_aiv, "nuts2_drivers_clean_aiv.rds")


library(readr)
nuts2_drivers_aiv <- readRDS("nuts2_drivers_clean_aiv.rds")
View(nuts2_drivers_aiv)

#This is for the Biocheck data for the poultry
# 1. Clean helper: summarise numeric columns only
clean_metric_table <- function(df) {
  df %>%
    group_by(NAME_LATN) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
}

#-------Down the BioCheck for the poulty-------------
Free_range_layers_Mean_Value <- readRDS("Free_range_layers_Mean_Value.rds")
Free_range_broilers_Mean_Value <- readRDS("Free_range_broilers_Mean_Value.rds")
Broilers_Mean_Value <- readRDS("Broilers_Mean_Value.rds")
Laying_hens_Mean_Value <- readRDS("Laying_hens_Mean_Value.rds")


# 2. Clean each metric table
Free_range_layer_Mean_Value_clean       <- clean_metric_table(Free_range_layers_Mean_Value)
Free_range_broilers_Mean_Value_clean      <- clean_metric_table(Free_range_broilers_Mean_Value)
Broilers_Mean_Value_clean       <- clean_metric_table(Broilers_Mean_Value)
Laying_hens_Mean_Value_clean      <- clean_metric_table(Laying_hens_Mean_Value)

# 3. Join all to nuts2_drivers
nuts2_drivers_biocheck_aiv <- nuts2_drivers_aiv %>%
  left_join(Free_range_layer_Mean_Value_clean,       by = "NAME_LATN") %>%
  left_join(Free_range_broilers_Mean_Value_clean,      by = "NAME_LATN") %>%
  left_join(Broilers_Mean_Value_clean,       by = "NAME_LATN") %>%
  left_join(Laying_hens_Mean_Value_clean,      by = "NAME_LATN") 


# 4. Save the above 
saveRDS(st_drop_geometry(nuts2_drivers_biocheck_aiv), "nuts2_drivers_biocheck_aiv.rds")

nuts2_drivers_biocheck_aiv <- readRDS("nuts2_drivers_biocheck_aiv.rds")


#-------------------------------------------
#AIV_Poultry data
AIV_Poultry_Birds <- readRDS("AIV_Poultry_Birds.rds")

AIV_Poultry_Birds <- AIV_Poultry_Birds %>%
  dplyr::group_by(NAME_LATN) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::mutate(Bird_cases= sum(cases, na.rm = T)) %>% #mean
  dplyr::select(NAME_LATN,Bird_cases) %>% 
  distinct()

nuts2_drivers_biocheck_AIV <- nuts2_drivers_biocheck_aiv %>%
  left_join(AIV_Poultry_Birds, by = "NAME_LATN")

saveRDS(nuts2_drivers_biocheck_AIV, file = "~/Desktop/Biosecure_Project-Hotspot-Mapping_Final_Jeron/nuts2_drivers_biocheck_AIV.rds")

#####################NOW EVERYTHING IS SET SO START FROM HERE #########################
nuts2_drivers_biocheck_AIV_2 <- readRDS("nuts2_drivers_biocheck_AIV.rds")

risk_data_used <- nuts2_drivers_biocheck_AIV_2 # Not loss the original cleaned data
risk_data <- nuts2_drivers_biocheck_AIV_2 

#Now to find or calculate risk scores

#Now to find the find actual probability of disease introduction:
# Logistic Regression for Probability Estimation
# 1. Logistic regression is a statistical model that predicts the probability of a binary outcome (e.g., disease introduction: yes/no) based on predictor variables.
# 2. The model outputs a value between 0 and 1 for each observation, which can be interpreted as the probability of disease introduction.


# --- Load required libraries ---
library(glmmTMB)
library(dplyr)

# --- Step 1: Ensure Bird_cases is numeric ---
risk_data$Bird_cases <- as.integer(as.character(risk_data$Bird_cases))

# --- Step 2: Define predictors ---
independent_vars <- c(
  "bird_density", "land_cover", "Free_range_layers_Mean_value", 
  "Free_range_broilers_Mean_value", "Broilers_Mean_value", 
  "Laying_hens_Mean_value"
)

# --- Step 3: Prepare model data ---
X <- risk_data[, independent_vars]
y <- risk_data$Bird_cases

# Impute missing values with median
X_imputed <- as.data.frame(lapply(X, function(col) {
  col[is.na(col)] <- median(col, na.rm = TRUE)
  col
}))

# Apply log1p transform
X_log <- as.data.frame(lapply(X_imputed, log1p))

# Combine predictors and response
data_model_clean <- cbind(X_log, Bird_cases = y)
data_model_clean <- na.omit(data_model_clean)

# --- Step 4: Fit Negative Binomial model using glmmTMB ---
formula_nb <- Bird_cases ~ bird_density + land_cover + 
  Free_range_layers_Mean_value + Free_range_broilers_Mean_value +
  Broilers_Mean_value + Laying_hens_Mean_value 

nb_model_tmb <- glmmTMB(
  formula = formula_nb,
  data = data_model_clean,
  family = nbinom2(link = "log")
)

# --- Step 5: Estimate theta via profile likelihood ---
profile_nb <- profile(nb_model_tmb, parm = "sigma")
confint_theta <- confint(profile_nb)

# Extract log(theta) confidence interval and convert to theta scale
log_theta_ci <- confint_theta["disp~(Intercept)", ]
theta_ci <- exp(log_theta_ci)

# Extract theta estimate
theta_hat <- sigma(nb_model_tmb)

# --- Step 6: Compute risk scores ---
coefs <- fixef(nb_model_tmb)$cond
coefs_no_intercept <- coefs[names(coefs) != "(Intercept)"]

X_full <- X_log[, names(coefs_no_intercept), drop = FALSE]
risk_scores_raw <- as.vector(as.matrix(X_full) %*% coefs_no_intercept)

normalize_score <- function(x) {
  100 * (x - min(x)) / (max(x) - min(x))
}
risk_score <- normalize_score(risk_scores_raw)
risk_data$risk_score <- risk_score

# --- Step 7: Predict disease probability ---
mu_hat_all <- predict(nb_model_tmb, newdata = X_log, type = "response")
prob_intro_all <- 1 - (theta_hat / (theta_hat + mu_hat_all))^theta_hat
risk_data$prob_intro_all <- prob_intro_all

# --- Step 8: Create new_cases and introduced columns ---
risk_data$time_within_location <- ave(risk_data$NUTS_ID, risk_data$NUTS_ID, FUN = seq_along)
risk_data$new_cases <- NA_integer_
risk_data$introduced <- NA_integer_

for(nid in unique(risk_data$NUTS_ID)) {
  idx <- which(risk_data$NUTS_ID == nid)
  cases <- risk_data$Bird_cases[idx]
  
  if(length(cases) == 1) {
    risk_data$new_cases[idx] <- cases
    risk_data$introduced[idx] <- as.integer(cases > 0)
  } else {
    risk_data$new_cases[idx] <- c(NA, diff(cases))
    risk_data$introduced[idx] <- as.integer(c(FALSE, head(cases, -1) == 0 & tail(cases, -1) > 0))
  }
}

risk_data$new_cases[is.na(risk_data$new_cases)] <- risk_data$Bird_cases[is.na(risk_data$new_cases)]

# --- Step 9: Final results dataframe ---
results <- data.frame(
  risk_data[, c("NUTS_ID", "NAME_LATN", "CNTR_CODE", 
                "time_within_location", independent_vars)],
  risk_score = risk_data$risk_score,
  prob_intro_all = risk_data$prob_intro_all,
  Bird_cases = y,
  new_cases = risk_data$new_cases,
  Disease_introduced = risk_data$introduced
)

# --- Step 10: Inspect results ---
head(results)


# ---- 6. Join with map (include CNTR_CODE!) ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")
# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 = nuts2_sf

nuts2_risk_sf <- nuts2_sf %>%
  left_join(results, by = "NUTS_ID")


# Write to rds
saveRDS(nuts2_risk_sf, file = "nuts2_risk_sf_aiv.rds")

nuts2_risk_sf_aiv <- readRDS("nuts2_risk_sf_aiv.rds")


#---------Plots---------------
# Risk score map
p_risk <- ggplot(nuts2_risk_sf_aiv) +
  geom_sf(aes(fill = risk_score), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "AIV Risk Score by NUTS2 Region",
    fill = "Risk Score"
  ) +
  coord_sf(
    xlim = c(-10, 55),   # longitude: west to east
    ylim = c(35, 70)     # latitude: south to north
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Print to viewer
print(p_risk)

# Create folder if it doesn't exist
if (!dir.exists("AIV_Figures")) {
  dir.create("AIV_Figures")
}

# Save the plot to the folder
ggsave(
  filename = "AIV_Figures/asf_risk_score_map.png",
  plot = p_risk,
  width = 10,
  height = 8,
  dpi = 300
)


# ---- 7. Analysis pipeline ----

# 7.1 Rank high-risk regions
top10 <- nuts2_risk_sf_aiv %>%
  arrange(desc(risk_score)) %>%
  dplyr::select(NUTS_ID, risk_score) %>%
  head(10)
print(top10)


#7.3 Scenario Comparison (Control Strategy) 
#First, define 
# Baseline risk data
risk_data_base <- nuts2_risk_sf_aiv

# Then create a "control scenario" where biocheck improves:
# Scenario: improve biosecurity by +0.2 (max 1.0)
# Load necessary libraries
# Load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

# Baseline risk score
risk_data_baseline <- risk_data_base %>%
  mutate(
    risk_score = rowSums(scale(cbind(
      Bird_cases,
      bird_density,
      land_cover,
      1 - Free_range_layers_Mean_value,
      1 - Free_range_broilers_Mean_value,
      1 - Broilers_Mean_value,
      1 - Laying_hens_Mean_value
    )), na.rm = TRUE)
  )

# 20% biosecurity increase
risk_data_boost <- risk_data_base %>%
  mutate(
    Free_range_layers_Mean_value = pmin(Free_range_layers_Mean_value + 0.2, 1.0),
    Free_range_broilers_Mean_value = pmin(Free_range_broilers_Mean_value + 0.2, 1.0),
    Broilers_Mean_value = pmin(Broilers_Mean_value + 0.2, 1.0),
    Laying_hens_Mean_value = pmin(Laying_hens_Mean_value + 0.2, 1.0)
  ) %>%
  mutate(
    risk_score = rowSums(scale(cbind(
      Bird_cases,
      bird_density,
      land_cover,
      1 - Free_range_layers_Mean_value,
      1 - Free_range_broilers_Mean_value,
      1 - Broilers_Mean_value,
      1 - Laying_hens_Mean_value
    )), na.rm = TRUE)
  )

# 20% biosecurity decrease
risk_data_decrease <- risk_data_base %>%
  mutate(
    Free_range_layers_Mean_value = pmax(Free_range_layers_Mean_value - 0.2, 0.0),
    Free_range_broilers_Mean_value = pmax(Free_range_broilers_Mean_value - 0.2, 0.0),
    Broilers_Mean_value = pmax(Broilers_Mean_value - 0.2, 0.0),
    Laying_hens_Mean_value = pmax(Laying_hens_Mean_value - 0.2, 0.0)
  ) %>%
  mutate(
    risk_score = rowSums(scale(cbind(
      Bird_cases,
      bird_density,
      land_cover,
      1 - Free_range_layers_Mean_value,
      1 - Free_range_broilers_Mean_value,
      1 - Broilers_Mean_value,
      1 - Laying_hens_Mean_value
    )), na.rm = TRUE)
  )


# Join to spatial data
nuts2_baseline_sf <- nuts2_sf %>%
  left_join(st_drop_geometry(risk_data_baseline) %>% dplyr::select(NUTS_ID, risk_score), by = "NUTS_ID")

nuts2_boost_sf <- nuts2_sf %>%
  left_join(st_drop_geometry(risk_data_boost) %>% dplyr::select(NUTS_ID, risk_score), by = "NUTS_ID")

nuts2_decrease_sf <- nuts2_sf %>%
  left_join(st_drop_geometry(risk_data_decrease) %>% dplyr::select(NUTS_ID, risk_score), by = "NUTS_ID")


# Boost difference
diff_boost_sf <- nuts2_boost_sf %>%
  left_join(st_drop_geometry(nuts2_baseline_sf) %>% dplyr::select(NUTS_ID, risk_score_baseline = risk_score), by = "NUTS_ID") %>%
  mutate(risk_score_diff = risk_score_baseline - risk_score)

# Decrease difference
diff_decrease_sf <- nuts2_decrease_sf %>%
  left_join(st_drop_geometry(nuts2_baseline_sf) %>% dplyr::select(NUTS_ID, risk_score_baseline = risk_score), by = "NUTS_ID") %>%
  mutate(risk_score_diff = risk_score_baseline - risk_score)

c_risk_aiv <- ggplot(diff_boost_sf) +
  geom_sf(aes(fill = risk_score_diff), color = "grey80", size = 0.1) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Δ Risk Score"
  ) +
  labs(
    title = "Change in AIV Risk Score After 20% Biosecurity Boost"#,
    #subtitle = "Blue = Decreased Risk | Red = Increased Risk",
    #caption = "Source: Biosecure Project"
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(c_risk_aiv)

d_risk_aiv <- ggplot(diff_decrease_sf) +
  geom_sf(aes(fill = risk_score_diff), color = "grey80", size = 0.1) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Δ Risk Score"
  ) +
  labs(
    title = "Change in AIV Risk Score After 20% Biosecurity Decrease"#,
    #subtitle = "Red = Increased Risk | Blue = Decreased Risk",
    #caption = "Source: Biosecure Project"
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(d_risk_aiv )

# Save the plot to the folder
ggsave(
  filename = "AIV_Figures/Risk Score After 20% Biosecurity Increase_map.png",
  plot = c_risk_aiv,
  width = 10,
  height = 8,
  dpi = 300
)

# Save the plot to the folder
ggsave(
  filename = "AIV_Figures/Risk Score After 20% Biosecurity Decrease_map.png",
  plot = d_risk_aiv,
  width = 10,
  height = 8,
  dpi = 300
)


#---------------------------------------------------------------------
library(spdep)
# 7.5 Local Spatial autocorrelation (Moran's I)
# Create neighbors

coords <- st_centroid(nuts2_risk_sf_aiv) %>% st_coordinates()

# Create distance-based neighbors within 100 km (100000 meters)
nb_dist <- dnearneigh(coords, 0, 150000)

lw_dist <- nb2listw(nb_dist, zero.policy = TRUE)

# Moran's I with distance-based weights
moran_dist <- moran.test(nuts2_risk_sf$risk_score, lw_dist, zero.policy = TRUE)
print(moran_dist)

#----------Plotting of the Disease Introduction------------------
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(cowplot)
library(stringr)

# --- Step 1: Join results to shapefile ---
nuts2_risk_sf_1 <- nuts2_sf %>%
  left_join(
    results %>%
      dplyr::select(NUTS_ID, risk_score, Disease_introduced),
    by = "NUTS_ID"
  )

# --- Step 2: Ensure Disease_introduced is numeric/factor ---
nuts2_risk_sf_1 <- nuts2_risk_sf_1 %>%
  mutate(
    Disease_introduced = as.integer(Disease_introduced),
    Disease_introduced_f = factor(Disease_introduced, levels = c(0,1), labels = c("Absent","Present")),
    risk_fill = if_else(Disease_introduced == 1, risk_score, NA_real_)
  )

# --- Step 3: Centroids for map labels ---
nuts2_centroids <- st_point_on_surface(nuts2_risk_sf_1)
nuts2_centroids_coords <- cbind(nuts2_centroids, st_coordinates(nuts2_centroids$geometry))

# --- Step 4: Map plot ---
p_map <- ggplot(nuts2_risk_sf_1) +
  geom_sf(aes(fill = risk_fill), color = "white", size = 0.15) +
  scale_fill_gradient(low = "pink", high = "darkred", na.value = "grey90", name = "Predicted Risk Score") +
  geom_text_repel(
    data = subset(nuts2_centroids_coords, Disease_introduced == 1),
    aes(X, Y, label = NUTS_ID),
    size = 3.5,
    fontface = "bold",
    color = "red",
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(t = -25, unit = "pt"),      # reduces space between map & legend
    legend.box.margin = margin(t = -25, unit = "pt"),  # fine-tune legend box
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")
  ) +
  labs(
    title = "Disease Introduction and Predicted Risk by NUTS-2 Region",
    subtitle = "Only regions with disease introduction are labeled"
  )

# --- Step 5: Side table ---
nuts2_table_1 <- nuts2_risk_sf_1 %>%
  filter(Disease_introduced == 1) %>%
  st_drop_geometry() %>%
  dplyr::select(NUTS_ID, NUTS_NAME, risk_score) %>%
  arrange(desc(risk_score)) %>%
  mutate(
    label = paste0(NUTS_ID, " - ", str_wrap(NUTS_NAME, 20), " (", round(risk_score, 2), ")")
  )

# Keep only top 15 regions
nuts2_table_1 <- nuts2_table_1 %>%
  slice_max(order_by = risk_score, n = 15)

p_table_1 <- ggplot(nuts2_table_1, aes(
  x = 1,
  y = reorder(NUTS_ID, -risk_score),
  label = label,
  color = risk_score
)) +
  geom_text(hjust = 0, size = 4, fontface = "bold", lineheight = 1.1) +
  scale_color_gradient(low = "pink", high = "darkred") +
  theme_void() +
  labs(title = "Top 15 Regions with Disease Introduced") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# --- Step 6: Combine using ggdraw for tighter layout ---
final_plot <- ggdraw() +
  draw_plot(p_map, x = 0, y = 0, width = 0.75, height = 1) +
  draw_plot(p_table_1, x = 0.75, y = 0.05, width = 0.25, height = 0.9)

print(final_plot)

# --- Step 7: Save high-resolution output ---
ggsave(
  filename = "AIV_Figures/disease_risk_map_with_side_legend_tight.png",
  plot = final_plot,
  width = 18,
  height = 12,
  dpi = 300
)

#------------------Prob Disease Introduction------------------------------------
# Load required libraries
library(glmmTMB)
library(MASS)

# Extract fitted values and theta
mu_hat <- fitted(nb_model_tmb)
theta_hat <- sigma(nb_model_tmb)

# Estimate theta uncertainty via profile likelihood
profile_nb <- profile(nb_model_tmb, parm = "sigma")
confint_theta <- confint(profile_nb)
log_theta_ci <- confint_theta["disp~(Intercept)", ]
log_se_theta <- diff(log_theta_ci) / (2 * 1.96)
log_se_theta <- mean(log_se_theta)  # use average if asymmetric

# Simulate theta values using log-normal distribution
set.seed(123)
n_sim <- 10000
log_theta_hat <- log(theta_hat)
theta_sim <- rlnorm(n_sim, meanlog = log_theta_hat, sdlog = log_se_theta)
theta_sim <- pmax(theta_sim, 0.01)  # enforce positivity

# Build a smooth grid of mu values
mu_seq <- seq(min(mu_hat), max(mu_hat), length.out = 200)

# Vectorized probability calculation
prob_sim_grid <- 1 - (outer(mu_seq, theta_sim, function(mu, th) (th / (th + mu))^th))
prob_sim_grid[!is.finite(prob_sim_grid)] <- NA

# Mean curve
prob_mean <- 1 - (theta_hat / (theta_hat + mu_seq))^theta_hat

# Confidence intervals
prob_lower_95 <- apply(prob_sim_grid, 1, quantile, 0.025, na.rm = TRUE)
prob_upper_95 <- apply(prob_sim_grid, 1, quantile, 0.975, na.rm = TRUE)
prob_lower_50 <- apply(prob_sim_grid, 1, quantile, 0.25, na.rm = TRUE)
prob_upper_50 <- apply(prob_sim_grid, 1, quantile, 0.75, na.rm = TRUE)

# Plot
plot(mu_seq, prob_mean,
     type = "l", lwd = 2, col = "#D55E00",
     xlab = "Expected number of cases",
     ylab = "Probability of disease introduction",
     main = "Probability of Disease Introduction",
     ylim = c(0.45, 1))
grid()

# Reference lines
abline(h = seq(0.75, 1, by = 0.05), col = "gray80", lty = "dotted")

# 95% CI band
polygon(c(mu_seq, rev(mu_seq)),
        c(prob_lower_95, rev(prob_upper_95)),
        col = rgb(0.85, 0.33, 0.0, 0.2), border = NA)

# 50% CI band
polygon(c(mu_seq, rev(mu_seq)),
        c(prob_lower_50, rev(prob_upper_50)),
        col = rgb(0.85, 0.33, 0.0, 0.4), border = NA)

# Mean curve again
lines(mu_seq, prob_mean, col = "#D55E00", lwd = 2)

# Observed points
points(mu_hat, 1 - (theta_hat / (theta_hat + mu_hat))^theta_hat,
       pch = 19, col = "#0072B2")

# Optional legend
legend("bottomright",
       legend = c("Mean", "50% CI", "95% CI", "Observed"),
       col = c("#D55E00", rgb(0.85, 0.33, 0.0, 0.4), rgb(0.85, 0.33, 0.0, 0.2), "#0072B2"),
       pch = c(NA, 15, 15, 19),
       lty = c(1, NA, NA, NA),
       pt.cex = c(NA, 2, 2, 1),
       bty = "n")
       
