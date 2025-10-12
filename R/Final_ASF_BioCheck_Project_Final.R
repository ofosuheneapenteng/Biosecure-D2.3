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
#--Indoor Pigs based on countries-- 2023/2024 --------------
X2023_reg_Biocheck_EU <- read_excel("2023_reg_Biocheck_EU.xlsx")
colnames(X2023_reg_Biocheck_EU)
unique(X2023_reg_Biocheck_EU$Country)

Biocheck_Region_InPigMean_2023 <- X2023_reg_Biocheck_EU %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  #dplyr::rename(ADMIN = Country) %>% 
  distinct() 

X2024_reg_Biocheck_EU <- read_excel("2024_reg_Biocheck_EU.xlsx", sheet= "Pigs_indoor")
colnames(X2024_reg_Biocheck_EU)
unique(X2024_reg_Biocheck_EU$Country)

Biocheck_Region_InPigMean_2024 <- X2024_reg_Biocheck_EU %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  #dplyr::group_by(Country,question ) %>% #dplyr::group_by(Country) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  #dplyr::rename(ADMIN = Country) %>% 
  distinct() 

df1 = Biocheck_Region_InPigMean_2023
df2 = Biocheck_Region_InPigMean_2024

#Sum their value columns, treating missing values as 0.
# Full outer join using full_join
full_merged <- full_join(df1, df2, by = c("ID","NAME_LATN"), suffix = c("_df1", "_df2"))

# Replace NAs with 0 and sum the values
Indoor_Mean_Value <- full_merged %>%
  mutate(
    Mean_score_Region_df1 = replace_na(Mean_score_Region_df1, 0),
    Mean_score_Region_df2 = replace_na(Mean_score_Region_df2, 0),
    Indoor_Mean_value = Mean_score_Region_df1 + Mean_score_Region_df2
  ) %>%
  dplyr::select(ID, NAME_LATN, Indoor_Mean_value)%>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

print(Indoor_Mean_Value )

saveRDS(Indoor_Mean_Value, file = "Indoor_Mean_Value.rds")


##-------Outdoor-------------------------
V2024_reg_Biocheck_EU <- read_excel("2024_reg_Biocheck_EU.xlsx", sheet= "Pigs_outdoor")
colnames(V2024_reg_Biocheck_EU)
unique(V2024_reg_Biocheck_EU$Country)

Biocheck_Region_OutPigMean_2024 <- V2024_reg_Biocheck_EU %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L")) %>% # Select multiple multiple questions
  dplyr::mutate(Outdoor_Mean_value= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Outdoor_Mean_value ) %>% 
  #dplyr::rename(ADMIN = Country) %>% 
  distinct() 

Outdoor_Mean_Value <- Biocheck_Region_OutPigMean_2024 %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

saveRDS(Outdoor_Mean_Value, file = "Outdoor_Mean_Value.rds")
#-----------------------------------------------------------------------------------------
# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")
# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 = nuts2_sf

#--------Drivers---------
library(exactextractr)


##------- Pig density----------------------------
#── Load Eurostat pig counts
# Load, clean, and summarize the data
nuts2pigs <- read_excel("Pig_density.xlsx") %>%
  rename(NUTS_ID = geo) %>%
  mutate(
    pig_count = as.numeric(gsub("[^0-9.]", "", value))  # remove non-numeric characters
  ) %>%
  group_by(NUTS_ID) %>%
  summarise(pig_density = sum(pig_count, na.rm = TRUE))

# Drop geometry if necessary (only if it's an sf object)
nuts2pigs_clean <- st_drop_geometry(nuts2pigs)

# Save as .rds file
saveRDS(nuts2pigs_clean, file = "nuts2_pig_density.rds")

nuts2_pig_density <- readRDS("nuts2_pig_density.rds")

#── Join pig counts to NUTS2
nuts2_pigs <- nuts2 %>%
  left_join(nuts2_pig_density, by = "NUTS_ID")

nuts2land_cover <- read_excel("land_cover.xlsx") %>%
  dplyr::select(NAME_LATN, value) %>%
  rename(land_cover = value) %>%
  mutate(land_cover = as.numeric(land_cover)) %>%
  group_by(NAME_LATN) %>%
  summarise(land_cover = mean(land_cover, na.rm = TRUE))

# Save as RDS
saveRDS(nuts2land_cover, file = "nuts2_land_cover.rds")

#--------------------Download the save drivers

df_pig   <- st_drop_geometry(readRDS("nuts2_pig_density.rds"))

nuts2_combined <- nuts2 %>%
  left_join(df_pig,   by = "NUTS_ID") 


library(sf)
library(dplyr)

# 1. Load land cover data and drop geometry
df_cover <- st_drop_geometry(readRDS("nuts2_land_cover.rds"))

# 2. Check land cover keys
if (!"NAME_LATN" %in% names(df_cover)) {
  stop("`NAME_LATN` not found in df_cover")
}

# 3. Fix nuts2_combined: rename if needed
if ("NAME_LATN.x" %in% names(nuts2_combined)) {
  nuts2_combined <- nuts2_combined %>%
    rename(NAME_LATN = NAME_LATN.x)
}

if (!"NAME_LATN" %in% names(nuts2_combined)) {
  stop("`NAME_LATN` not found in nuts2_combined after renaming")
}

# 4. Join land cover data
nuts2_joined <- nuts2_combined %>%
  left_join(df_cover, by = "NAME_LATN")

# 5. Remove duplicate columns with .x and .y suffixes created by joins
nuts2_clean <- nuts2_joined %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"))

# 6. Drop geometry (if any)
nuts2_clean <- nuts2_clean %>%
  sf::st_drop_geometry()

# 7. Select only the final columns needed
nuts2_drivers <- nuts2_clean %>%
  dplyr::select(
    NUTS_ID, CNTR_CODE, NAME_LATN,
    pig_density, land_cover
  )

# 8. Save final cleaned data
saveRDS(nuts2_drivers, "nuts2_drivers_clean.rds")


library(readr)
nuts2_drivers <- readRDS("nuts2_drivers_clean.rds")
View(nuts2_drivers)

#This is for the Biocheck data which have been cleaned 
# 1. Clean helper: summarise numeric columns only
clean_metric_table <- function(df) {
  df %>%
    group_by(NAME_LATN) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
}

Indoor_Mean_Value <- readRDS("Indoor_Mean_Value.rds")
Outdoor_Mean_Value <- readRDS("Outdoor_Mean_Value.rds")

# 2. Clean each metric table
Indoor_Mean_Value_clean       <- clean_metric_table(Indoor_Mean_Value)
Outdoor_Mean_Value_clean      <- clean_metric_table(Outdoor_Mean_Value)

# 3. Join all to nuts2_drivers
nuts2_drivers_biocheck <- nuts2_drivers %>%
  left_join(Indoor_Mean_Value_clean,       by = "NAME_LATN") %>%
  left_join(Outdoor_Mean_Value_clean,      by = "NAME_LATN") 


# 4. Export to GeoPackage and CSV
saveRDS(st_drop_geometry(nuts2_drivers_biocheck), "nuts2_drivers_biocheck.rds")

nuts2_drivers_biocheck <- readRDS("nuts2_drivers_biocheck.rds")


#-------------------------------------------
#ASF-Pig data
ASF_Swine_Pigs <- read_excel("ASF_Swine_Pigs.xlsx")

ASF_Swine_Pigs_Nice <- ASF_Swine_Pigs %>%
  dplyr::group_by(NAME_LATN) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::mutate(Pig_cases= sum(cases, na.rm = T)) %>% #mean
  dplyr::select(NAME_LATN,Pig_cases) %>% 
  distinct()

nuts2_drivers_biocheck_ASF <- nuts2_drivers_biocheck %>%
  left_join(ASF_Swine_Pigs_Nice, by = "NAME_LATN")%>%
  #st_drop_geometry() %>%
  dplyr::select(NUTS_ID, NAME_LATN, CNTR_CODE,pig_density,land_cover,
                Indoor_Mean_value,Outdoor_Mean_value,Pig_cases)

saveRDS(st_drop_geometry( nuts2_drivers_biocheck_ASF), " nuts2_drivers_biocheck_ASF.rds")

nuts2_drivers_biocheck_ASF <- readRDS(" nuts2_drivers_biocheck_ASF.rds")

risk_data <- nuts2_drivers_biocheck_ASF 

#Now to find or calculate risk scores

#Now to find the find actual probability of disease introduction:
# Logistic Regression for Probability Estimation
# 1. Logistic regression is a statistical model that predicts the probability of a binary outcome (e.g., disease introduction: yes/no) based on predictor variables.
# 2. The model outputs a value between 0 and 1 for each observation, which can be interpreted as the probability of disease introduction.


# --- Load required libraries ---
library(MASS)   # For glm.nb
library(car)    # For VIF and alias detection

# --- Step 0: Ensure Pig_cases is numeric ---
risk_data$Pig_cases <- as.integer(as.character(risk_data$Pig_cases))

# --- Step 1: Define predictors ---
independent_vars <- c("pig_density", "land_cover",
                      "Indoor_Mean_value", "Outdoor_Mean_value")

# --- Step 2: Extract predictors (X) and response (y) ---
X <- risk_data[, independent_vars]
y <- risk_data$Pig_cases

# --- Step 3: Impute missing values in predictors with median ---
X_imputed <- as.data.frame(lapply(X, function(col) {
  col[is.na(col)] <- median(col, na.rm = TRUE)
  col
}))

# --- Step 4: Apply log1p transform to predictors ---
X_log <- as.data.frame(lapply(X_imputed, log1p))

# --- Step 5: Impute any remaining NA values after log1p ---
X_log[] <- lapply(X_log, function(col) {
  col[is.na(col)] <- median(col, na.rm = TRUE)
  col
})

# --- Step 6: Combine into model data frame ---
data_model <- cbind(X_log, Pig_cases = y)

# --- Step 7: Remove rows with NA in response ---
data_model_clean <- na.omit(data_model)

# --- Step 8: Detect and remove aliased variables using Poisson model ---
poisson_model <- glm(Pig_cases ~ ., data = data_model_clean, family = "poisson")
aliased_vars <- rownames(alias(poisson_model)$Complete)
if (length(aliased_vars) > 0) {
  data_model_pruned <- data_model_clean[, !colnames(data_model_clean) %in% aliased_vars]
} else {
  data_model_pruned <- data_model_clean
}

# --- Step 9: Fit Negative Binomial model ---
nb_model <- glm.nb(Pig_cases ~ ., data = data_model_pruned, control = glm.control(maxit = 100))

# --- Step 9b: Compute risk scores (linear predictor scaled 0–100) ---
coefs <- coef(nb_model)
coefs_no_intercept <- coefs[!names(coefs) %in% "(Intercept)"]

X_full <- X_log[, names(coefs_no_intercept), drop = FALSE]
risk_scores_raw <- as.vector(as.matrix(X_full) %*% coefs_no_intercept)

normalize_score <- function(x) { 
  100 * (x - min(x)) / (max(x) - min(x)) 
}
risk_score <- normalize_score(risk_scores_raw)
risk_data$risk_score <- risk_score

# --- Step 10: Predict disease probability ---
mu_hat_all <- predict(nb_model, newdata = X_log, type = "response")
theta <- nb_model$theta
prob_intro_all <- 1 - (theta / (theta + mu_hat_all))^theta
risk_data$prob_intro_all <- prob_intro_all

# --- Step 11: Create robust new_cases and introduced columns ---
risk_data$time_within_location <- ave(risk_data$NUTS_ID, risk_data$NUTS_ID, FUN = seq_along)
risk_data$new_cases <- NA_integer_
risk_data$introduced <- NA_integer_

for(nid in unique(risk_data$NUTS_ID)) {
  idx <- which(risk_data$NUTS_ID == nid)
  cases <- risk_data$Pig_cases[idx]
  
  if(length(cases) == 1) {
    # Single row: cross-sectional
    risk_data$new_cases[idx] <- cases
    risk_data$introduced[idx] <- as.integer(cases > 0)
  } else {
    # Multiple rows: longitudinal
    risk_data$new_cases[idx] <- c(NA, diff(cases))
    risk_data$introduced[idx] <- as.integer(c(FALSE, head(cases, -1) == 0 & tail(cases, -1) > 0))
  }
}

# Optional: Treat first observation in each location as new case
risk_data$new_cases[is.na(risk_data$new_cases)] <- risk_data$Pig_cases[is.na(risk_data$new_cases)]

# --- Step 12: Final results dataframe ---
results <- data.frame(
  risk_data[, c("NUTS_ID", "NAME_LATN", "CNTR_CODE", 
                "time_within_location", independent_vars)],
  risk_score = risk_data$risk_score,
  prob_intro_all = risk_data$prob_intro_all,
  Pig_cases = y,
  new_cases = risk_data$new_cases,
  Disease_introduced = risk_data$introduced
)

# --- Step 13: Inspect results ---
head(results)


# Write to rds
saveRDS(results, file = "results_pig.rds")

results_pig <- readRDS("results_pig.rds") 
results <- results_pig 

# ---- 6. Join with map (include CNTR_CODE!) ----
nuts2_risk_sf <- nuts2_sf %>%
  left_join(results, by = "NUTS_ID")

# Write to rds
saveRDS(nuts2_risk_sf, file = "nuts2_risk_sf.rds") 

nuts2_risk_sf <- readRDS("nuts2_risk_sf.rds") 

# Risk score map
p_risk <- ggplot(nuts2_risk_sf) +
  geom_sf(aes(fill = risk_score), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "ASF Risk Score by NUTS2 Region",
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
if (!dir.exists("ASF_Figures")) {
  dir.create("ASF_Figures")
}

# Save the plot to the folder
ggsave(
  filename = "ASF_Figures/asf_risk_score_map.png",
  plot = p_risk,
  width = 10,
  height = 8,
  dpi = 300
)


# Pig density map
p_pigdensity <- ggplot(nuts2_risk_sf) +
  geom_sf(aes(fill = pig_density), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") + #"plasma"
  labs(title = "Pig Density by NUTS2 Region", 
       fill = "Pig Density") +
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
print(p_pigdensity)

# Save the plot to the folder
ggsave(
  filename = "ASF_Figures/pig_density_map.png",
  plot = p_pigdensity,
  width = 10,
  height = 8,
  dpi = 300
)

# ---- 7. Analysis pipeline ----

# 7.1 Rank high-risk regions
top10 <- nuts2_risk_sf %>%
  arrange(desc(risk_score)) %>%
  dplyr::select(NUTS_ID, risk_score) %>%
  head(10)
print(top10)

top10_prob <- nuts2_risk_sf %>%
  arrange(desc(Prob_disease_intro)) %>%
  dplyr::select(NUTS_ID, Prob_disease_intro) %>%
  head(10)
print(top10_prob)

#7.3 Scenario Comparison (Control Strategy) 
#First, define 
# Baseline risk data
risk_data_base <- results

# Then create a "control scenario" where biocheck improves:
# Scenario: improve biosecurity by +0.2 and -0.2 (max 1.0)
# Load necessary libraries
# Load necessary libraries
library(dplyr)
library(sf)
library(ggplot2)
library(readr)

risk_data_base <- results
# Step 1: Baseline risk score
risk_data_baseline <- risk_data_base %>%
  mutate(
    risk_score = rowSums(scale(cbind(
      Pig_cases,
      pig_density,
      land_cover,
      1 - Indoor_Mean_value,
      1 - Outdoor_Mean_value
    )), na.rm = TRUE)
  )

# Step 2: Apply 20% biosecurity boost
risk_data_boost <- risk_data_base %>%
  mutate(
    Indoor_Mean_value  = pmin(Indoor_Mean_value + 0.2, 1),
    Outdoor_Mean_value = pmin(Outdoor_Mean_value + 0.2, 1)
  ) %>%
  mutate(
    risk_score = rowSums(scale(cbind(
      Pig_cases,
      pig_density,
      land_cover,
      1 - Indoor_Mean_value,
      1 - Outdoor_Mean_value
    )), na.rm = TRUE)
  )

# Step 3: Apply 20% biosecurity decrease
risk_data_decrease <- risk_data_base %>%
  mutate(
    Indoor_Mean_value  = pmax(Indoor_Mean_value - 0.2, 0),
    Outdoor_Mean_value = pmax(Outdoor_Mean_value - 0.2, 0)
  ) %>%
  mutate(
    risk_score = rowSums(scale(cbind(
      Pig_cases,
      pig_density,
      land_cover,
      1 - Indoor_Mean_value,
      1 - Outdoor_Mean_value
    )), na.rm = TRUE)
  )

# Step 4: Join to spatial data
nuts2_baseline_sf <- nuts2_sf %>%
  left_join(st_drop_geometry(risk_data_baseline) %>% dplyr::select(NUTS_ID, risk_score), by = "NUTS_ID")

nuts2_boost_sf <- nuts2_sf %>%
  left_join(st_drop_geometry(risk_data_boost) %>% dplyr::select(NUTS_ID, risk_score), by = "NUTS_ID")

nuts2_decrease_sf <- nuts2_sf %>%
  left_join(st_drop_geometry(risk_data_decrease) %>% dplyr::select(NUTS_ID, risk_score), by = "NUTS_ID")

# Step 5: Compute differences
diff_boost_sf <- nuts2_boost_sf %>%
  left_join(st_drop_geometry(nuts2_baseline_sf) %>% dplyr::select(NUTS_ID, risk_score_baseline = risk_score), by = "NUTS_ID") %>%
  mutate(risk_score_diff = risk_score_baseline - risk_score)

diff_decrease_sf <- nuts2_decrease_sf %>%
  left_join(st_drop_geometry(nuts2_baseline_sf) %>% dplyr::select(NUTS_ID, risk_score_baseline = risk_score), by = "NUTS_ID") %>%
  mutate(risk_score_diff = risk_score_baseline - risk_score)

# Step 6: Plot boost scenario

boost_plot <- ggplot(diff_boost_sf) +
  geom_sf(aes(fill = risk_score_diff), color = "grey80", size = 0.1) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Δ Risk Score"
  ) +
  labs(
    title = "Change in ASF Risk Score After 20% Biosecurity Boost"
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(boost_plot)

ggsave("ASF_Figures/ASF_Risk_Biosecurity_Boost_20pct_map.png", plot = boost_plot, width = 10, height = 8, dpi = 300)


# Step 7: Plot decrease scenario
decrease_plot <- ggplot(diff_decrease_sf) +
  geom_sf(aes(fill = risk_score_diff), color = "grey80", size = 0.1) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0,
    name = "Δ Risk Score"
  ) +
  labs(
    title = "Change in ASF Risk Score After 20% Biosecurity Decrease"#,
    #subtitle = "Blue = Increased Risk | Violet = Decreased Risk"
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(decrease_plot)
# Step 5: Save the plot
ggsave(
  filename = "ASF_Figures/ASF_Risk_Biosecurity_Decrease_20pct_map.png",
  plot = decrease_plot,
  width = 10,
  height = 8,
  dpi = 300
)

#---------------------------------------------------------------------

library(spdep)
# 7.5 Spatial autocorrelation (Moran's I)
# Create neighbors

coords <- st_centroid(nuts2_risk_sf) %>% st_coordinates()

# Create distance-based neighbors within 100 km (100000 meters)
nb_dist <- dnearneigh(coords, 0, 150000)

lw_dist <- nb2listw(nb_dist, zero.policy = TRUE)

# Moran's I with distance-based weights
moran_dist <- moran.test(nuts2_risk_sf$risk_score, lw_dist, zero.policy = TRUE)
print(moran_dist)


#----------------------------Plot of Disease_intro----------------------------------------
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(cowplot)
library(stringr)

# --- Step 1: Join results to shapefile ---
nuts2_risk_sf <- nuts2_sf %>%
  left_join(
    results %>%
      dplyr::select(NUTS_ID, risk_score, Disease_introduced),
    by = "NUTS_ID"
  )

# --- Step 2: Ensure Disease_introduced is numeric/factor ---
nuts2_risk_sf <- nuts2_risk_sf %>%
  mutate(
    Disease_introduced = as.integer(Disease_introduced),
    Disease_introduced_f = factor(Disease_introduced, levels = c(0,1), labels = c("Absent","Present")),
    risk_fill = if_else(Disease_introduced == 1, risk_score, NA_real_)
  )

# --- Step 3: Centroids for map labels ---
nuts2_centroids <- st_point_on_surface(nuts2_risk_sf)
nuts2_centroids_coords <- cbind(nuts2_centroids, st_coordinates(nuts2_centroids$geometry))

# --- Step 4: Map plot ---
p_map <- ggplot(nuts2_risk_sf) +
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
nuts2_table <- nuts2_risk_sf %>%
  filter(Disease_introduced == 1) %>%
  st_drop_geometry() %>%
  dplyr::select(NUTS_ID, NUTS_NAME, risk_score) %>%
  arrange(desc(risk_score)) %>%
  mutate(
    label = paste0(NUTS_ID, " - ", str_wrap(NUTS_NAME, 20), " (", round(risk_score, 2), ")")
  )

# Keep only top 15 regions
nuts2_table <- nuts2_table %>%
  slice_max(order_by = risk_score, n = 15)

p_table <- ggplot(nuts2_table, aes(
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
  draw_plot(p_table, x = 0.75, y = 0.05, width = 0.25, height = 0.9)

print(final_plot)

# --- Step 7: Save high-resolution output ---
ggsave(
  filename = "ASF_Figures/disease_risk_map_with_side_legend_tight.png",
  plot = final_plot,
  width = 18,
  height = 12,
  dpi = 300
)

#------------------Prob Disease Introduction------------------------------------
# After fitting your model
library(MASS)

# Extract fitted values and theta
mu_hat <- fitted(nb_model)
theta_hat <- nb_model$theta
se_theta <- summary(nb_model)$SE.theta  

# Simulate theta and trim to 15th–85th percentile
set.seed(123)
n_sim <- 10000
theta_sim <- rnorm(n_sim, mean = theta_hat, sd = se_theta)
theta_sim <- theta_sim[theta_sim > 0.01]  # enforce positivity
theta_sim <- theta_sim[
  theta_sim > quantile(theta_sim, 0.15) &
    theta_sim < quantile(theta_sim, 0.85)
]

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
prob_lower_50 <- apply(prob_sim_grid, 1, quantile, 0.30, na.rm = TRUE)
prob_upper_50 <- apply(prob_sim_grid, 1, quantile, 0.70, na.rm = TRUE)

# Plot
plot(mu_seq, prob_mean,
     type = "l", lwd = 2, col = "#D55E00",
     xlab = "Expected number of cases",
     ylab = "Probability of disease introduction",
     main = "Probability of Disease Introduction",
     ylim = c(0.8, 1))
grid()
abline(h = seq(0.75, 1, by = 0.05), col = "gray80", lty = "dotted")

# CI bands with reduced alpha
polygon(c(mu_seq, rev(mu_seq)),
        c(prob_lower_95, rev(prob_upper_95)),
        col = rgb(0.85, 0.33, 0.0, 0.1), border = NA)
polygon(c(mu_seq, rev(mu_seq)),
        c(prob_lower_50, rev(prob_upper_50)),
        col = rgb(0.85, 0.33, 0.0, 0.25), border = NA)

# Mean curve and observed points
lines(mu_seq, prob_mean, col = "#D55E00", lwd = 2)
points(mu_hat, 1 - (theta_hat / (theta_hat + mu_hat))^theta_hat,
       pch = 19, col = "#0072B2")

# Legend
legend("bottomright",
       legend = c("Mean", "50% CI", "95% CI", "Observed"),
       col = c("#D55E00", rgb(0.85, 0.33, 0.0, 0.25), rgb(0.85, 0.33, 0.0, 0.1), "#0072B2"),
       pch = c(NA, 15, 15, 19), lty = c(1, NA, NA, NA),
       pt.cex = c(NA, 2, 2, 1), bty = "n")