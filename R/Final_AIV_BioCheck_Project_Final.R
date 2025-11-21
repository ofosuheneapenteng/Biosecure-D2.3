
rm(list = objects())

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(broom.mixed)

# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")

# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 <- nuts2_sf

# Load data
AI_nuts2_outbreaks <- readRDS("AI_nuts2_outbreaks_poultry.rds")

#------------Plotting the biosecurity values--------------
# 
# # Poultry density
p_poultry <- ggplot(AI_nuts2_outbreaks) +
  geom_sf(aes(fill = poultry_density), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "Poultry density Level by NUTS2 Region",
    fill = bquote("Head / km"^2)
  ) +
  coord_sf(
    xlim = c(-10, 55),
    ylim = c(35, 70)
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

print(p_poultry)


# Step 17: Save the plot
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

ggsave(
  filename = "AI_Risk_Figures/Poultry_Correct density.png",
  plot = p_poultry,
  width = 10,
  height = 8,
  dpi = 300
)


# Free_range_layers_Mean_value
p_FRL <- ggplot(AI_nuts2_outbreaks) +
  geom_sf(aes(fill = Free_range_layers_Mean_value), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") + #"plasma"
  labs(title = "Free range layers mean value for birds by NUTS2 Region",
       caption = "Grey = all NUTS2 regions without free range layers mean value.",
       fill = "Free_range_layers_Mean_value") +
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
print(p_FRL)

# Step 17: Save the plot
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

ggsave(
  filename = "AI_Risk_Figures/Free_range_layers_Mean_value.png",
  plot = p_FRL,
  width = 10,
  height = 8,
  dpi = 300
)


# Outdoor mean value
p_FRB <- ggplot(AI_nuts2_outbreaks) +
  geom_sf(aes(fill = Free_range_broilers_Mean_value), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") + #"plasma"
  labs(title = "Free range broilers mean value for birds by NUTS2 Region",
       caption = "Grey = all NUTS2 regions without free range broilers mean value.",
       fill = "Free_range_broilers_Mean_value") +
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
print(p_FRB)


# Step 17: Save the plot
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

ggsave(
  filename = "AI_Risk_Figures/Free range broilers mean value.png",
  plot = p_FRB,
  width = 10,
  height = 8,
  dpi = 300
)


# Broilers_Mean_value
p_Broiler <- ggplot(AI_nuts2_outbreaks) +
  geom_sf(aes(fill = Broilers_Mean_value), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") + #"plasma"
  labs(title = "Broilers mean value for birds by NUTS2 Region",
       caption = "Grey = all NUTS2 regions without broilers mean value.",
       fill = "Broilers_Mean_value") +
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
print(p_Broiler)

# Step 17: Save the plot
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

ggsave(
  filename = "AI_Risk_Figures/Broilers mean value.png",
  plot = p_Broiler,
  width = 10,
  height = 8,
  dpi = 300
)


# Laying_hens_Mean_value
p_Laying <- ggplot(AI_nuts2_outbreaks) +
  geom_sf(aes(fill = Laying_hens_Mean_value), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") + #"plasma" , limits = c(0, 100)
  labs(title = "Laying hen mean value for birds by NUTS2 Region",
       caption = "Grey = all NUTS2 regions without laying hens mean value.",
       fill = "Laying_hens_Mean_value") +
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
print(p_Laying)

# Step 17: Save the plot
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

ggsave(
  filename = "AI_Risk_Figures/Laying_hens_Mean_value.png",
  plot = p_Laying,
  width = 10,
  height = 8,
  dpi = 300
)


# land_cover_name
p_land <- ggplot(AI_nuts2_outbreaks) +
  geom_sf(aes(fill = land_cover_code), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno") + #"plasma" , limits = c(0, 100)
  labs(title = "Land cover by NUTS2 Region",
       caption = "Grey = all NUTS2 regions without land cover.",
       fill = "Land cover ") +
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
print(p_land)

# Step 17: Save the plot
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

ggsave(
  filename = "AI_Risk_Figures/land_cover_code.png",
  plot = p_land,
  width = 10,
  height = 8,
  dpi = 300
)


#-----------Start from here-------------
# Step 1: Impute missing values and decrease broiler and laying hen values using the baseline
AI_nuts2_outbreaks_baseline <- AI_nuts2_outbreaks %>%
  mutate(
    Free_range_layers_Mean_value = ifelse(is.na(Free_range_layers_Mean_value),
                                          mean(Free_range_layers_Mean_value, na.rm = TRUE), Free_range_layers_Mean_value),
    Free_range_broilers_Mean_value = ifelse(is.na(Free_range_broilers_Mean_value),
                                            mean(Free_range_broilers_Mean_value, na.rm = TRUE), Free_range_broilers_Mean_value),
    Broilers_Mean_value = ifelse(is.na(Broilers_Mean_value),
                                 mean(Broilers_Mean_value, na.rm = TRUE), Broilers_Mean_value),
    Laying_hens_Mean_value = ifelse(is.na(Laying_hens_Mean_value),
                                    mean(Laying_hens_Mean_value, na.rm = TRUE), Laying_hens_Mean_value)
  )

# Step 2: Filter complete cases
AI_model_data_baseline <- AI_nuts2_outbreaks_baseline %>%
  filter(!is.na(outbreak_count),
         !is.na(poultry_density),
         !is.na(chicken_density),
         !is.na(Free_range_layers_Mean_value),
         !is.na(land_cover_name),
         !is.na(Free_range_broilers_Mean_value),
         !is.na(Broilers_Mean_value),
         !is.na(Laying_hens_Mean_value))

# Step 3: Create binary outcome
AI_model_data_baseline <- AI_model_data_baseline %>%
  mutate(outbreak_binary = ifelse(outbreak_count > 0, 1, 0))

# Step 4: Sample 50 cases and 50 controls
cases <- AI_model_data_baseline %>% filter(outbreak_binary == 1)
controls_all <- AI_model_data_baseline %>% filter(outbreak_binary == 0)

n_samples <- 50
set.seed(123)
sampled_cases <- sample_n(cases, n_samples)
sampled_controls <- sample_n(controls_all, n_samples, replace = TRUE)

AI_model_data_balanced_baseline <- bind_rows(sampled_cases, sampled_controls)

# Step 5: Group land cover
AI_model_data_balanced_baseline <- AI_model_data_balanced_baseline %>%
  mutate(grouped_land_cover = case_when(
    land_cover_name %in% c("Arable land", "Pastures", "Permanent crops") ~ "Agricultural",
    land_cover_name %in% c("Forest", "Scrub and/or herbaceous vegetation associations", "Open spaces with little or no vegetation") ~ "Natural",
    land_cover_name %in% c("Urban fabric", "Industrial units", "Green urban areas", "Sport and leisure facilities") ~ "Urban",
    land_cover_name %in% c("Wetlands", "Water bodies") ~ "Water-related",
    TRUE ~ NA_character_
  )) %>%
  mutate(grouped_land_cover = factor(grouped_land_cover))

# Step 6: Fit logistic regression model
logit_model_baseline <- glm(
  outbreak_binary ~ poultry_density + grouped_land_cover + Broilers_Mean_value + Laying_hens_Mean_value,
  data = AI_model_data_balanced_baseline,
  family = binomial(link = "logit")
) #chicken_density +

# Step 7: Get odds ratios and 95% confidence intervals
odds_ratio_table_case_baseline <- broom.mixed::tidy(
  logit_model_baseline,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

# Step 8: View results
summary(logit_model_baseline)
print(odds_ratio_table_case_baseline)

# Step 9: Predict risk scores
AI_model_data_balanced_baseline$risk_score <- predict(logit_model_baseline, newdata = AI_model_data_balanced_baseline, type = "response")

# Step 10: Extract coordinates
AI_model_data_balanced_baseline <- AI_model_data_balanced_baseline %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],
    lat = st_coordinates(st_centroid(geometry))[, 2]
  ) %>%
  st_drop_geometry()

# Step 11: Convert to sf and spatial join
AI_sf_baseline <- st_as_sf(AI_model_data_balanced_baseline, coords = c("lon", "lat"), crs = 4326)
AI_joined_baseline <- st_join(AI_sf_baseline, nuts2, join = st_intersects)

# Step 12: Aggregate risk scores by NUTS2 region
nuts2_risk_baseline <- AI_joined_baseline %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID.y) %>%
  summarise(mean_risk = mean(risk_score, na.rm = TRUE))

nuts2_map_baseline <- left_join(nuts2, nuts2_risk_baseline, by = c("NUTS_ID" = "NUTS_ID.y"))

# Step 13: Identify regions with outbreak OR control AND risk score
regions_with_condition <- AI_joined_baseline %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID.y) %>%
  summarise(has_outbreak = any(outbreak_binary == 1),
            has_control = any(outbreak_binary == 0),
            risk_available = any(!is.na(risk_score))) %>%
  filter((has_outbreak | has_control) & risk_available) %>%
  pull(NUTS_ID.y)

# Step 14: Remove Malta (MT00)
regions_with_condition <- regions_with_condition[regions_with_condition != "MT00"]

# Step 15: Filter map and AI points
nuts2_filtered_baseline <- nuts2_map_baseline %>%
  filter(NUTS_ID %in% regions_with_condition & is.finite(mean_risk))

AI_filtered_baseline <- AI_joined_baseline %>%
  filter(NUTS_ID.y %in% nuts2_filtered_baseline$NUTS_ID & !is.na(risk_score))  # ✅ Filter here

# Step 16: Plot (only points with valid risk scores)
b_equl_baseline <- ggplot() +
  geom_sf(data = nuts2_map_baseline, fill = "lightgrey", color = "white") +
  geom_sf(data = nuts2_filtered_baseline, aes(fill = mean_risk), color = "white") +
  geom_sf(data = AI_filtered_baseline |> filter(outbreak_binary == 1), 
          aes(color = "Outbreak Cases"), shape = 16, size = 2) +
  geom_sf(data = AI_filtered_baseline |> filter(outbreak_binary == 0), 
          aes(color = "Control Regions"), shape = 16, size = 2) +
  scale_fill_viridis_c(name = "Risk score", option = "inferno") +
  scale_color_manual(
    name = "Outbreak Classification",
    values = c("Outbreak Cases" = "red", "Control Regions" = "blue")
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  labs(
    title = "Al Risk Map (Regions with Outbreak OR Control & Risk)",
    caption = "Grey = all NUTS2 regions; colored = regions with outbreak OR control and valid risk score."
  )

print(b_equl_baseline)

# Step 17: Save the plot
ggsave(
  filename = "AI_Risk_Figures/ai_poultry_both_only_baseline.png",
  plot = b_equl_baseline,
  width = 10,
  height = 8,
  dpi = 300
)

#----------------------20% Increased-----------------------------
rm(list = objects())

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(broom.mixed)

# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")

# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 <- nuts2_sf

# Load data
AI_nuts2_outbreaks <- readRDS("AI_nuts2_outbreaks_poultry.rds")

# Step 1: Impute missing values and decrease broiler and laying hen values by 20%
AI_nuts2_outbreaks_up <- AI_nuts2_outbreaks %>%
  mutate(
    Free_range_layers_Mean_value = ifelse(is.na(Free_range_layers_Mean_value),
                                          mean(Free_range_layers_Mean_value, na.rm = TRUE), Free_range_layers_Mean_value),
    Free_range_broilers_Mean_value = ifelse(is.na(Free_range_broilers_Mean_value),
                                            mean(Free_range_broilers_Mean_value, na.rm = TRUE), Free_range_broilers_Mean_value),
    Broilers_Mean_value = ifelse(is.na(Broilers_Mean_value),
                                 mean(Broilers_Mean_value, na.rm = TRUE), Broilers_Mean_value) * 1.2,
    Laying_hens_Mean_value = ifelse(is.na(Laying_hens_Mean_value),
                                    mean(Laying_hens_Mean_value, na.rm = TRUE), Laying_hens_Mean_value) * 1.2
  )

# Step 2: Filter complete cases
AI_model_data_up <- AI_nuts2_outbreaks_up %>%
  filter(!is.na(outbreak_count),
         !is.na(poultry_density),
         !is.na(chicken_density),
         !is.na(Free_range_layers_Mean_value),
         !is.na(land_cover_name),
         !is.na(Free_range_broilers_Mean_value),
         !is.na(Broilers_Mean_value),
         !is.na(Laying_hens_Mean_value))

# Step 3: Create binary outcome
AI_model_data_up <- AI_model_data_up %>%
  mutate(outbreak_binary = ifelse(outbreak_count > 0, 1, 0))

# Step 4: Sample 50 cases and 50 controls
cases <- AI_model_data_up %>% filter(outbreak_binary == 1)
controls_all <- AI_model_data_up %>% filter(outbreak_binary == 0)

n_samples <- 50
set.seed(123)
sampled_cases <- sample_n(cases, n_samples)
sampled_controls <- sample_n(controls_all, n_samples, replace = TRUE)

AI_model_data_balanced_up <- bind_rows(sampled_cases, sampled_controls)

# Step 5: Group land cover
AI_model_data_balanced_up <- AI_model_data_balanced_up %>%
  mutate(grouped_land_cover = case_when(
    land_cover_name %in% c("Arable land", "Pastures", "Permanent crops") ~ "Agricultural",
    land_cover_name %in% c("Forest", "Scrub and/or herbaceous vegetation associations", "Open spaces with little or no vegetation") ~ "Natural",
    land_cover_name %in% c("Urban fabric", "Industrial units", "Green urban areas", "Sport and leisure facilities") ~ "Urban",
    land_cover_name %in% c("Wetlands", "Water bodies") ~ "Water-related",
    TRUE ~ NA_character_
  )) %>%
  mutate(grouped_land_cover = factor(grouped_land_cover))

# Step 6: Fit logistic regression model
logit_model_up <- glm(
  outbreak_binary ~ poultry_density + chicken_density + grouped_land_cover + Broilers_Mean_value + Laying_hens_Mean_value,
  data = AI_model_data_balanced_up,
  family = binomial(link = "logit")
)

# Step 7: Get odds ratios and 95% confidence intervals
odds_ratio_table_case_up <- broom.mixed::tidy(
  logit_model_up,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

# Step 8: View results
summary(logit_model_up)
print(odds_ratio_table_case_up)

# Step 9: Predict risk scores
AI_model_data_balanced_up$risk_score <- predict(logit_model_up, newdata = AI_model_data_balanced_up, type = "response")

# Step 10: Extract coordinates
AI_model_data_balanced_up <- AI_model_data_balanced_up %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],
    lat = st_coordinates(st_centroid(geometry))[, 2]
  ) %>%
  st_drop_geometry()

# Step 11: Convert to sf and spatial join
AI_sf_up <- st_as_sf(AI_model_data_balanced_up, coords = c("lon", "lat"), crs = 4326)
AI_joined_up <- st_join(AI_sf_up, nuts2, join = st_intersects)

# Step 12: Aggregate risk scores by NUTS2 region
nuts2_risk_up <- AI_joined_up %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID.y) %>%
  summarise(mean_risk = mean(risk_score, na.rm = TRUE))

nuts2_map_up <- left_join(nuts2, nuts2_risk_up, by = c("NUTS_ID" = "NUTS_ID.y"))

# Step 13: Identify regions with outbreak OR control AND risk score
regions_with_condition <- AI_joined_up %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID.y) %>%
  summarise(has_outbreak = any(outbreak_binary == 1),
            has_control = any(outbreak_binary == 0),
            risk_available = any(!is.na(risk_score))) %>%
  filter((has_outbreak | has_control) & risk_available) %>%
  pull(NUTS_ID.y)

# Step 14: Remove Malta (MT00)
regions_with_condition <- regions_with_condition[regions_with_condition != "MT00"]

# Step 15: Filter map and AI points
nuts2_filtered_up <- nuts2_map_up %>%
  filter(NUTS_ID %in% regions_with_condition & is.finite(mean_risk))

AI_filtered_up <- AI_joined_up %>%
  filter(NUTS_ID.y %in% nuts2_filtered_up$NUTS_ID & !is.na(risk_score))  # ✅ Filter here

# Step 16: Plot (only points with valid risk scores)
b_equl_up <- ggplot() +
  geom_sf(data = nuts2_map_up, fill = "lightgrey", color = "white") +
  geom_sf(data = nuts2_filtered_up, aes(fill = mean_risk), color = "white") +
  geom_sf(data = AI_filtered_up |> filter(outbreak_binary == 1), 
          aes(color = "Outbreak Cases"), shape = 16, size = 2) +
  geom_sf(data = AI_filtered_up |> filter(outbreak_binary == 0), 
          aes(color = "Control Regions"), shape = 16, size = 2) +
  scale_fill_viridis_c(name = "Risk score", option = "inferno") +
  scale_color_manual(
    name = "Outbreak Classification",
    values = c("Outbreak Cases" = "red", "Control Regions" = "blue")
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  labs(
    title = "HPAI Risk Map (20% Increase in Broilers & Laying Hens)",
    caption = "Grey = all NUTS2 regions; colored = regions with outbreak OR control and valid risk score."
  )

print(b_equl_up)

# Step 17: Save the plot
ggsave(
  filename = "AI_Risk_Figures/ai_poultry_both_only_20_up.png",
  plot = b_equl_up,
  width = 10,
  height = 8,
  dpi = 300
) 


#--------------------------------------- 20% Decreased-----------------------------


rm(list = objects())

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(broom.mixed)

# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")

# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 <- nuts2_sf

# Load data
AI_nuts2_outbreaks <- readRDS("AI_nuts2_outbreaks_poultry.rds")

# Step 1: Impute missing values and decrease broiler and laying hen values by 20%
AI_nuts2_outbreaks_down <- AI_nuts2_outbreaks %>%
  mutate(
    Free_range_layers_Mean_value = ifelse(is.na(Free_range_layers_Mean_value),
                                          mean(Free_range_layers_Mean_value, na.rm = TRUE), Free_range_layers_Mean_value),
    Free_range_broilers_Mean_value = ifelse(is.na(Free_range_broilers_Mean_value),
                                            mean(Free_range_broilers_Mean_value, na.rm = TRUE), Free_range_broilers_Mean_value),
    Broilers_Mean_value = ifelse(is.na(Broilers_Mean_value),
                                 mean(Broilers_Mean_value, na.rm = TRUE), Broilers_Mean_value) * 0.8,
    Laying_hens_Mean_value = ifelse(is.na(Laying_hens_Mean_value),
                                    mean(Laying_hens_Mean_value, na.rm = TRUE), Laying_hens_Mean_value) * 0.8
  )

# Step 2: Filter complete cases
AI_model_data_down <- AI_nuts2_outbreaks_down %>%
  filter(!is.na(outbreak_count),
         !is.na(poultry_density),
         !is.na(chicken_density),
         !is.na(Free_range_layers_Mean_value),
         !is.na(land_cover_name),
         !is.na(Free_range_broilers_Mean_value),
         !is.na(Broilers_Mean_value),
         !is.na(Laying_hens_Mean_value))

# Step 3: Create binary outcome
AI_model_data_down <- AI_model_data_down %>%
  mutate(outbreak_binary = ifelse(outbreak_count > 0, 1, 0))

# Step 4: Sample 50 cases and 50 controls
cases <- AI_model_data_down %>% filter(outbreak_binary == 1)
controls_all <- AI_model_data_down %>% filter(outbreak_binary == 0)

n_samples <- 50
set.seed(123)
sampled_cases <- sample_n(cases, n_samples)
sampled_controls <- sample_n(controls_all, n_samples, replace = TRUE)

AI_model_data_balanced_down <- bind_rows(sampled_cases, sampled_controls)

# Step 5: Group land cover
AI_model_data_balanced_down <- AI_model_data_balanced_down %>%
  mutate(grouped_land_cover = case_when(
    land_cover_name %in% c("Arable land", "Pastures", "Permanent crops") ~ "Agricultural",
    land_cover_name %in% c("Forest", "Scrub and/or herbaceous vegetation associations", "Open spaces with little or no vegetation") ~ "Natural",
    land_cover_name %in% c("Urban fabric", "Industrial units", "Green urban areas", "Sport and leisure facilities") ~ "Urban",
    land_cover_name %in% c("Wetlands", "Water bodies") ~ "Water-related",
    TRUE ~ NA_character_
  )) %>%
  mutate(grouped_land_cover = factor(grouped_land_cover))

# Step 6: Fit logistic regression model
logit_model_down <- glm(
  outbreak_binary ~ poultry_density + chicken_density + grouped_land_cover + Broilers_Mean_value + Laying_hens_Mean_value,
  data = AI_model_data_balanced_down,
  family = binomial(link = "logit")
)

# Step 7: Get odds ratios and 95% confidence intervals
odds_ratio_table_case_down <- broom.mixed::tidy(
  logit_model_down,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

# Step 8: View results
summary(logit_model_down)
print(odds_ratio_table_case_down)

# Step 9: Predict risk scores
AI_model_data_balanced_down$risk_score <- predict(logit_model_down, newdata = AI_model_data_balanced_down, type = "response")

# Step 10: Extract coordinates
AI_model_data_balanced_down <- AI_model_data_balanced_down %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],
    lat = st_coordinates(st_centroid(geometry))[, 2]
  ) %>%
  st_drop_geometry()

# Step 11: Convert to sf and spatial join
AI_sf_down <- st_as_sf(AI_model_data_balanced_down, coords = c("lon", "lat"), crs = 4326)
AI_joined_down <- st_join(AI_sf_down, nuts2, join = st_intersects)

# Step 12: Aggregate risk scores by NUTS2 region
nuts2_risk_down <- AI_joined_down %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID.y) %>%
  summarise(mean_risk = mean(risk_score, na.rm = TRUE))

nuts2_map_down <- left_join(nuts2, nuts2_risk_down, by = c("NUTS_ID" = "NUTS_ID.y"))

# Step 13: Identify regions with outbreak OR control AND risk score
regions_with_condition <- AI_joined_down %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID.y) %>%
  summarise(has_outbreak = any(outbreak_binary == 1),
            has_control = any(outbreak_binary == 0),
            risk_available = any(!is.na(risk_score))) %>%
  filter((has_outbreak | has_control) & risk_available) %>%
  pull(NUTS_ID.y)

# Step 14: Remove Malta (MT00)
regions_with_condition <- regions_with_condition[regions_with_condition != "MT00"]

# Step 15: Filter map and AI points
nuts2_filtered_down <- nuts2_map_down %>%
  filter(NUTS_ID %in% regions_with_condition & is.finite(mean_risk))

AI_filtered_down <- AI_joined_down %>%
  filter(NUTS_ID.y %in% nuts2_filtered_down$NUTS_ID & !is.na(risk_score))  # ✅ Filter here

# Step 16: Plot (only points with valid risk scores)
b_equl_down <- ggplot() +
  geom_sf(data = nuts2_map_down, fill = "lightgrey", color = "white") +
  geom_sf(data = nuts2_filtered_down, aes(fill = mean_risk), color = "white") +
  geom_sf(data = AI_filtered_down |> filter(outbreak_binary == 1), 
          aes(color = "Outbreak Cases"), shape = 16, size = 2) +
  geom_sf(data = AI_filtered_down |> filter(outbreak_binary == 0), 
          aes(color = "Control Regions"), shape = 16, size = 2) +
  scale_fill_viridis_c(name = "Risk score", option = "inferno") +
  scale_color_manual(
    name = "Outbreak Classification",
    values = c("Outbreak Cases" = "red", "Control Regions" = "blue")
  ) +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  labs(
    title = "HPAI Risk Map (20% Decrease in Broilers & Laying Hens)",
    caption = "Grey = all NUTS2 regions; colored = regions with outbreak OR control and valid risk score."
  )

print(b_equl_down)

# Step 17: Save the plot
ggsave(
  filename = "AI_Risk_Figures/ai_poultry_both_only_20_down.png",
  plot = b_equl_down,
  width = 10,
  height = 8,
  dpi = 300
)

