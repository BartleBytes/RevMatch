# ========================================================
# RevMatch Dataset Builder Script
# Purpose: Clean and engineer features for a motorcycle
# recommendation / utility optimization engine.
# ========================================================

library(tidyverse)
library(janitor)
library(readr)
library(scales)
library(naniar)

# =====================================================
# 1. Load Raw Moto Dataset
# =====================================================

moto_raw <- read_csv(
  "data/all_bikez_curated.csv",
  show_col_types = FALSE
)

names(moto_raw)
glimpse(moto_raw)
summary(moto_raw)

problems(moto_raw)

# =====================================================
# 2. Initial Filtering
# =====================================================

moto <- moto_raw %>%
  filter(
    Year >= 1990,
    !is.na(`Power (hp)`),
    !is.na(`Dry weight (kg)`),
    !is.na(`Seat height (mm)`),
    !is.na(`Fuel capacity (lts)`),
    !is.na(`Displacement (ccm)`),
    !is.na(Category)
  ) %>%
  filter(
    `Power (hp)` > 0,
    `Power (hp)` <= 300,
    `Dry weight (kg)` > 20,
    `Dry weight (kg)` <= 500,
    `Seat height (mm)` >= 400,
    `Seat height (mm)` <= 1200,
    `Fuel capacity (lts)` > 0,
    `Fuel capacity (lts)` <= 40,
    `Displacement (ccm)` > 0,
    `Displacement (ccm)` <= 3000
  )

# =====================================================
# 3. Category Cleanup
# =====================================================

moto <- moto %>%
  mutate(
    category_group = case_when(
      Category == "Scooter" ~ "Scooter",
      
      Category %in% c(
        "Naked bike",
        "Allround",
        "Classic"
      ) ~ "Standard / Naked",
      
      Category %in% c(
        "Sport",
        "Sport touring"
      ) ~ "Sport",
      
      Category == "Custom / cruiser" ~ "Cruiser",
      
      Category == "Touring" ~ "Touring",
      
      Category %in% c(
        "Enduro / offroad",
        "Super motard",
        "Trial"
      ) ~ "Adventure / Dual Sport",
      
      Category %in% c(
        "Cross / motocross",
        "Minibike, cross",
        "Minibike, sport"
      ) ~ "Dirt / Mini",
      
      Category == "ATV" ~ "ATV",
      
      TRUE ~ "Other"
    )
  )

sort(table(moto$Category), decreasing = TRUE)
sort(table(moto$category_group), decreasing = TRUE)

# =====================================================
# 4. Unit Conversion + Core Numeric Fields
# =====================================================

moto_model <- moto %>%
  mutate(
    horsepower = `Power (hp)`,
    torque_nm = `Torque (Nm)`,
    displacement_cc = `Displacement (ccm)`,
    
    weight_lbs = `Dry weight (kg)` * 2.20462,
    seat_height_in = `Seat height (mm)` / 25.4,
    fuel_capacity_gal = `Fuel capacity (lts)` * 0.264172,
    
    rating_clean = if_else(
      is.na(Rating),
      median(Rating, na.rm = TRUE),
      Rating
    )
  )

# =====================================================
# 5. Feature Engineering
# =====================================================

moto_model <- moto_model %>%
  mutate(
    power_to_weight = horsepower / weight_lbs,
    hp_per_cc = horsepower / displacement_cc,
    
    quality_score = rating_clean * 20,
    
    value_score = scales::rescale(
      hp_per_cc,
      to = c(0, 100)
    ),
    
    beginner_score = case_when(
      horsepower <= 50 & weight_lbs <= 425 ~ 100,
      horsepower <= 75 & weight_lbs <= 500 ~ 80,
      horsepower <= 100 ~ 60,
      TRUE ~ 20
    ),
    
    performance_score = scales::rescale(
      power_to_weight,
      to = c(0, 100)
    ),
    
    touring_score = case_when(
      category_group == "Touring" ~ 100,
      category_group == "Adventure / Dual Sport" ~ 80,
      fuel_capacity_gal >= 5 ~ 75,
      TRUE ~ 40
    ),
    
    commuting_score = case_when(
      category_group %in% c("Standard / Naked", "Scooter") &
        weight_lbs <= 450 ~ 90,
      
      category_group == "Sport" &
        weight_lbs <= 450 ~ 75,
      
      category_group == "Cruiser" ~ 60,
      
      TRUE ~ 50
    ),
    
    utility_base =
      0.25 * beginner_score +
      0.25 * performance_score +
      0.20 * touring_score +
      0.15 * commuting_score +
      0.15 * quality_score
  )

# =====================================================
# 6. Recommendation Feature Matrix
# =====================================================

feature_cols <- c(
  "horsepower",
  "torque_nm",
  "weight_lbs",
  "seat_height_in",
  "fuel_capacity_gal",
  "beginner_score",
  "performance_score",
  "touring_score",
  "commuting_score",
  "quality_score",
  "value_score"
)

moto_model <- moto_model %>%
  filter(
    if_all(
      all_of(feature_cols),
      ~ !is.na(.x) & is.finite(.x)
    )
  )

feature_matrix <- moto_model %>%
  select(all_of(feature_cols)) %>%
  scale()

# =====================================================
# 7. K-Means Clustering
# =====================================================

set.seed(123)

kmeans_fit <- kmeans(
  feature_matrix,
  centers = 8,
  nstart = 25
)

moto_model <- moto_model %>%
  mutate(
    cluster = kmeans_fit$cluster
  )

table(moto_model$cluster)

# =====================================================
# 8. PCA for Visualization
# =====================================================

pca_fit <- prcomp(feature_matrix)

pca_df <- tibble(
  Brand = moto_model$Brand,
  Model = moto_model$Model,
  Year = moto_model$Year,
  category_group = moto_model$category_group,
  cluster = factor(moto_model$cluster),
  PC1 = pca_fit$x[, 1],
  PC2 = pca_fit$x[, 2]
)

# Optional quick plot
ggplot(
  pca_df,
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Motorcycle Clusters",
    subtitle = "PCA projection of engineered recommendation features",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal()

# =====================================================
# 9. Validation Checks
# =====================================================

dim(moto_model)
glimpse(moto_model)

required_cols <- c(
  "Brand",
  "Model",
  "Year",
  "Category",
  "category_group",
  "horsepower",
  "torque_nm",
  "displacement_cc",
  "weight_lbs",
  "seat_height_in",
  "fuel_capacity_gal",
  "power_to_weight",
  "hp_per_cc",
  "quality_score",
  "value_score",
  "beginner_score",
  "performance_score",
  "touring_score",
  "commuting_score",
  "utility_base",
  "cluster"
)

setdiff(required_cols, names(moto_model))

colSums(is.na(moto_model[required_cols]))

moto_model %>%
  summarise(
    rows = n(),
    
    min_year = min(Year, na.rm = TRUE),
    max_year = max(Year, na.rm = TRUE),
    
    min_hp = min(horsepower, na.rm = TRUE),
    max_hp = max(horsepower, na.rm = TRUE),
    
    min_weight = min(weight_lbs, na.rm = TRUE),
    max_weight = max(weight_lbs, na.rm = TRUE),
    
    min_seat = min(seat_height_in, na.rm = TRUE),
    max_seat = max(seat_height_in, na.rm = TRUE),
    
    min_fuel = min(fuel_capacity_gal, na.rm = TRUE),
    max_fuel = max(fuel_capacity_gal, na.rm = TRUE),
    
    min_quality = min(quality_score, na.rm = TRUE),
    max_quality = max(quality_score, na.rm = TRUE),
    
    min_value = min(value_score, na.rm = TRUE),
    max_value = max(value_score, na.rm = TRUE),
    
    min_utility = min(utility_base, na.rm = TRUE),
    max_utility = max(utility_base, na.rm = TRUE)
  )

sort(table(moto_model$category_group), decreasing = TRUE)
sort(table(moto_model$cluster), decreasing = TRUE)

miss_var_summary(moto_model)

# =====================================================
# 10. Save Data
# =====================================================

dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

write_csv(
  moto_model,
  "data/clean/moto_model.csv"
)

write_csv(
  pca_df,
  "data/clean/moto_pca.csv"
)

saveRDS(
  kmeans_fit,
  "data/clean/kmeans_fit.rds"
)

saveRDS(
  pca_fit,
  "data/clean/pca_fit.rds"
)

