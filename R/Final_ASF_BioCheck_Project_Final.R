
1.1	Indoor Pig 2023 data
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

#Please change your working directory 
setwd("~/Desktop/Biosecure_Project-Hotspot-Mapping_Final")

#----Biosecurity Measurers For Both  -------
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

#1.2	Indoor Pig 2024 data
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

#1.3	Indoor Pig data combined 2023 and 2024

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


#1.4	Outdoor Pig 2024 data
# Note there was no data for Outdoor Pig in 2023 except 2024 only, which we received from Task 2.1
##-------Outdoor Pig -------------------------
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

#1.5	Pig density data
library(terra)
library(sf)

# Ensure pig_density is a SpatRaster
pig_density <- rast("Pig_density.tif")
# If nuts2_outbreaks is not already sf, convert it
nuts2_outbreaks <- st_as_sf(nuts2_outbreaks)
# Then convert to SpatVector
nuts2_vect <- vect(nuts2_outbreaks)
# Now extract pig density
pig_density_extract <- terra::extract(pig_density, nuts2_vect, fun = mean, na.rm = TRUE)
# Add pig density to original sf object
nuts2_outbreaks$pig_density <- pig_density_extract[, 2]  # Adjust column index if needed

#1.6	Wild boar density data

# Ensure wild boar_density is a SpatRaster
wild_boar_density <- rast("wildboar_density.tif")
# If nuts2_outbreaks is not already sf, convert it
nuts2_outbreaks <- st_as_sf(nuts2_outbreaks)
# Then convert to SpatVector
nuts2_vect <- vect(nuts2_outbreaks)
# Now extract pig density
wild_boar_density_extract <- terra::extract(wild_boar_density, nuts2_vect, fun = mean, na.rm = TRUE)
# Add pig density to original sf object
nuts2_outbreaks$wild_boar_density <- wild_boar_density_extract[, 2]  # Adjust column index if needed

#1.7	Land cover data with pig

# Step 1: Ensure land_cover is a SpatRaster
land_cover <- rast("corine_rat_LEVEL2.tif")

# Step 2: Convert nuts2_outbreaks to sf and then to SpatVector
nuts2_outbreaks <- st_as_sf(nuts2_outbreaks)
nuts2_vect <- vect(nuts2_outbreaks)

# Step 3: Extract modal land cover class per polygon
land_cover_extract <- extract(land_cover, nuts2_vect, fun = modal, na.rm = TRUE)

# Step 4: Assign numeric land cover code to nuts2_outbreaks
nuts2_outbreaks$land_cover_code <- land_cover_extract[, 2]  # Adjust index if needed

# Step 5: Create lookup table for LEVEL2 land cover codes
land_cover_lookup <- data.frame(
  code = c(11, 12, 13, 14, 21, 22, 23, 24, 25, 31, 32, 33, 41, 42, 51, 52),
  name = c("Urban fabric", "Industrial units", "Mine sites", "Artificial vegetation",
           "Arable land", "Permanent crops", "Pastures", "Heterogeneous agriculture",
           "Forests", "Scrub/herbaceous", "Open spaces", "Wetlands", "Inland wetlands",
           "Coastal wetlands", "Inland waters", "Marine waters")
)

# Step 6: Join lookup table to get readable names
nuts2_outbreaks <- nuts2_outbreaks %>%
  left_join(land_cover_lookup, by = c("land_cover_code" = "code"))

# Optional: Rename column for clarity
nuts2_outbreaks <- nuts2_outbreaks %>%
  rename(land_cover_name = name)

#1.8	ASF occurrence 2023 and 2024 data 
# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")
# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 = nuts2_sf

#ASF-Pig data
ASF_Swine_Pigs <- read_excel("ASF_Swine_Pigs.xlsx")

# Convert ASF outbreak data to sf object
ASF_Swine_Pigs_sf <- ASF_Swine_Pigs %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(nuts2))

# Spatial join: assign each outbreak to a NUTS2 region
ASF_with_NUTS2 <- st_join(ASF_Swine_Pigs_sf, nuts2, join = st_intersects)

# Aggregate outbreak counts by NUTS2 region
outbreak_counts <- ASF_with_NUTS2 %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID) %>%
  summarise(outbreak_count = n())

# Now join outbreak counts to nuts2 geometries
nuts2_outbreaks <- nuts2 %>%
  left_join(outbreak_counts, by = "NUTS_ID") %>%
  mutate(outbreak_count = replace_na(outbreak_count, 0))

# Create a new column to classify regions
nuts2_outbreaks <- nuts2_outbreaks %>%
  mutate(outbreak_status = ifelse(outbreak_count > 0, "Outbreak", "No Outbreak"))

# Plot the map
p_cases <- ggplot(nuts2_outbreaks) +
  geom_sf(aes(fill = outbreak_status), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Outbreak" = "red", "No Outbreak" = "lightgray")) +
  labs(title = "ASF Pig Outbreak Status by NUTS2 Region",
       fill = "Status") +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Print to viewer
print(p_cases)

# Create folder if it doesn't exist
if (!dir.exists("ASF_Risk_Figures")) {
  dir.create("ASF_Risk_Figures")
}

# Save the plot to the folder
ggsave(
  filename = "ASF_Risk_Figures/asf_pig_cases_map.png",
  plot = p_cases,
  width = 10,
  height = 8,
  dpi = 300
)

#1.9	Free-range layer data 2023
Biocheck_Region_Mean_Free_range_layers_2023 <- X2023_reg_Biocheck_EU_BIRDS_Free_range_layers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#1.10	Free-range broiler 2023 data
Biocheck_Region_Mean_Free_range_broilers_2023 <- X2023_reg_Biocheck_EU_BIRDS_Free_range_broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#1.11	Broiler 2023 data
Biocheck_Region_Mean_Broilers_2023 <- X2023_reg_Biocheck_EU_BIRDS_Broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#1.12	Laying hen 2023 data
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

#1.13	Free-range layers 2024 data
Biocheck_Region_Mean_Free_range_layers_2024 <- X2024_reg_Biocheck_EU_BIRDS_Free_range_layers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#1.14	Free-range broilers 2024 data
Biocheck_Region_Mean_Free_range_broilers_2024 <- X2024_reg_Biocheck_EU_BIRDS_Free_range_broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#1.15	Broilers 2024 data
Biocheck_Region_Mean_Broilers_2024 <- X2024_reg_Biocheck_EU_BIRDS_Broilers %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  distinct() 

#1.16	Laying hens 2024 data

Biocheck_Region_Mean_Laying_hens_2024 <- X2024_reg_Biocheck_EU_BIRDS_Laying_hens %>% 
  mutate(ID = sub("\\..*", "", question)) %>% 
  dplyr::group_by(NAME_LATN,ID) %>% #dplyr::group_by(Regional Level) %>% 
  dplyr::filter(ID %in% c("A","B","C","D","E", "F","G","H","I","J","K","L","M","N")) %>% # Select multiple multiple questions
  dplyr::mutate(Mean_score_Region= mean(`Mean value`, na.rm = T)) %>% 
  dplyr::select(ID,NAME_LATN,Mean_score_Region ) %>% 
  #dplyr::rename(ADMIN = Country) %>% 
  distinct() 

#1.17	Free-range layers data combined 2023 and 2024
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

#1.18	Free-range broilers data combined 2023 and 2024
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

#1.19	Broilers data combined 2023 and 2024
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

#1.20	Laying hens data combined 2023 and 2024
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

1.21	Cover Land Data with bird

# Step 1: Ensure land_cover is a SpatRaster
land_cover <- rast("corine_rat_LEVEL2.tif")

# Step 2: Convert nuts2_outbreaks to sf and then to SpatVector
nuts2_outbreaks <- st_as_sf(nuts2_outbreaks)
nuts2_vect <- vect(nuts2_outbreaks)

# Step 3: Extract modal land cover class per polygon
land_cover_extract <- extract(land_cover, nuts2_vect, fun = modal, na.rm = TRUE)

# Step 4: Assign numeric land cover code to nuts2_outbreaks
nuts2_outbreaks$land_cover_code <- land_cover_extract[, 2]  # Adjust index if needed

# Step 5: Create lookup table for LEVEL2 land cover codes
land_cover_lookup <- data.frame(
  code = c(11, 12, 13, 14, 21, 22, 23, 24, 25, 31, 32, 33, 41, 42, 51, 52),
  name = c("Urban fabric", "Industrial units", "Mine sites", "Artificial vegetation",
           "Arable land", "Permanent crops", "Pastures", "Heterogeneous agriculture",
           "Forests", "Scrub/herbaceous", "Open spaces", "Wetlands", "Inland wetlands",
           "Coastal wetlands", "Inland waters", "Marine waters")
)

# Step 6: Join lookup table to get readable names
nuts2_outbreaks <- nuts2_outbreaks %>%
  left_join(land_cover_lookup, by = c("land_cover_code" = "code"))

# Optional: Rename column for clarity
nuts2_outbreaks <- nuts2_outbreaks %>%
  rename(land_cover_name = name)

1.22	Bird Density Data
#--------------------------------------------------------------
#Read in the bio climate and land cover data
bird_density <- st_drop_geometry(readRDS("bird_density.rds"))

# 2. Check land cover keys
if (!"NAME_LATN" %in% names(bird_density)) {
  stop("`NAME_LATN` not found in df_bird")
}

# 3. Fix nuts2_combined: rename if needed
if ("NAME_LATN" %in% names(bird_density)) {
  bird_density <- bird_density %>%
    rename(bird_density = total_median_abundance)
}

nuts2_outbreaks_ai <- nuts2_outbreaks
# 4. Join birds density data to bird case
nuts2_outbreaks_ai <- nuts2_outbreaks_ai %>%
  left_join(bird_density, by = "NAME_LATN")

# 1.23	AI Poultry Occurrence 2023 and 2024 Data

# ---- 1. Download & unzip shapefile ----
nuts_sf <- st_read("NUTS_RG_01M_2021_4326.shp/NUTS_RG_01M_2021_4326.shp")
# ---- 2. Filter to NUTS2 ----
nuts2_sf <- nuts_sf %>% filter(LEVL_CODE == 2)
nuts2 = nuts2_sf

#AIV_Poultry data
AI_Poultry_Birds <- readRDS("AIV_Poultry_Birds.rds")

# Convert AI outbreak data to sf object
AI_Birds_sf <- AI_Poultry_Birds %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(nuts2))

# Spatial join: assign each outbreak to a NUTS2 region
AI_Bird_with_NUTS2 <- st_join(AI_Birds_sf, nuts2, join = st_intersects)

# Aggregate outbreak counts by NUTS2 region
outbreak_counts <- AI_Bird_with_NUTS2 %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID) %>%
  summarise(outbreak_count = n())

# Now join outbreak counts to nuts2 geometries
nuts2_outbreaks <- nuts2 %>%
  left_join(outbreak_counts, by = "NUTS_ID") %>%
  mutate(outbreak_count = replace_na(outbreak_count, 0))

# Create a new column to classify regions
nuts2_outbreaks_ai <- nuts2_outbreaks %>%
  mutate(outbreak_status = ifelse(outbreak_count > 0, "Outbreak", "No Outbreak"))


# Plot the map
b_cases <- ggplot(nuts2_outbreaks_ai) +
  geom_sf(aes(fill = outbreak_status), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Outbreak" = "red", "No Outbreak" = "lightgray")) +
  labs(title = "AI Bird Outbreak Status by NUTS2 Region",
       fill = "Status") +
  coord_sf(xlim = c(-10, 55), ylim = c(35, 70)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Print to viewer
print(b_cases)

# Create folder if it doesn't exist
if (!dir.exists("AI_Risk_Figures")) {
  dir.create("AI_Risk_Figures")
}

# Save the plot to the folder
ggsave(
  filename = "AI_Risk_Figures/asf_bird_cases_map.png",
  plot = b_cases,
  width = 10,
  height = 8,
  dpi = 300
)

