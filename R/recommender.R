# ========================================================
# RevMatch Recommendation Engine
# Purpose: Recommend motorcycles using utility scoring
# and cosine similarity.
# ========================================================

library(tidyverse)
library(scales)

# ========================================================
# 1. Feature Columns
# ========================================================

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

# ========================================================
# 2. Cosine Similarity
# ========================================================

cosine_similarity <- function(a, b) {
  sum(a * b) /
    (
      sqrt(sum(a^2)) *
        sqrt(sum(b^2))
    )
}

# ========================================================
# 3. Build User Vector
# ========================================================

build_user_vector <- function(
    experience = "Beginner",
    rider_height = 68,
    use_case = "Commuting",
    desired_hp = 50,
    desired_weight = 425
) {
  
  beginner_score <- case_when(
    experience == "Beginner" ~ 100,
    experience == "Intermediate" ~ 70,
    experience == "Advanced" ~ 30,
    TRUE ~ 70
  )
  
  performance_score <- case_when(
    use_case == "Performance" ~ 100,
    use_case == "Weekend Fun" ~ 80,
    use_case == "Commuting" ~ 50,
    use_case == "Touring" ~ 50,
    use_case == "Adventure" ~ 60,
    TRUE ~ 50
  )
  
  touring_score <- case_when(
    use_case == "Touring" ~ 100,
    use_case == "Adventure" ~ 80,
    TRUE ~ 40
  )
  
  commuting_score <- case_when(
    use_case == "Commuting" ~ 100,
    use_case == "Weekend Fun" ~ 70,
    TRUE ~ 50
  )
  
  tibble(
    horsepower = desired_hp,
    torque_nm = desired_hp * 0.90,
    weight_lbs = desired_weight,
    seat_height_in = rider_height * 0.45,
    fuel_capacity_gal = case_when(
      use_case == "Touring" ~ 5.5,
      use_case == "Adventure" ~ 5.0,
      TRUE ~ 3.8
    ),
    beginner_score = beginner_score,
    performance_score = performance_score,
    touring_score = touring_score,
    commuting_score = commuting_score,
    quality_score = 70,
    value_score = 70
  )
}

# ========================================================
# 4. Dynamic Utility Score
# ========================================================

add_utility_score <- function(
    df,
    experience,
    use_case,
    rider_height,
    desired_hp,
    desired_weight
) {
  
  df %>%
    mutate(
      hp_fit = pmax(
        0,
        100 - abs(horsepower - desired_hp)
      ),
      
      weight_fit = pmax(
        0,
        100 - abs(weight_lbs - desired_weight) / 3
      ),
      
      ergonomics_fit = pmax(
        0,
        100 - abs(seat_height_in - rider_height * 0.45) * 10
      ),
      
      experience_fit = case_when(
        experience == "Beginner" & horsepower <= 50 ~ 100,
        experience == "Beginner" & horsepower <= 75 ~ 80,
        experience == "Beginner" & horsepower <= 100 ~ 50,
        experience == "Beginner" ~ 10,
        
        experience == "Intermediate" & horsepower <= 100 ~ 100,
        experience == "Intermediate" & horsepower <= 130 ~ 80,
        experience == "Intermediate" ~ 50,
        
        experience == "Advanced" ~ 100,
        TRUE ~ 70
      ),
      
      use_case_fit = case_when(
        use_case == "Commuting" ~ commuting_score,
        use_case == "Touring" ~ touring_score,
        use_case == "Performance" ~ performance_score,
        use_case == "Adventure" & category_group == "Adventure / Dual Sport" ~ 100,
        use_case == "Weekend Fun" ~ pmax(performance_score, commuting_score),
        TRUE ~ 60
      ),
      
      utility_score =
        0.25 * use_case_fit +
        0.20 * experience_fit +
        0.20 * ergonomics_fit +
        0.15 * hp_fit +
        0.10 * weight_fit +
        0.05 * quality_score +
        0.05 * value_score
    )
}

# ========================================================
# 5. Explanation Engine
# ========================================================

create_explanation <- function(row) {
  
  reasons <- c()
  
  if (row$beginner_score >= 80) {
    reasons <- c(reasons, "beginner-friendly")
  }
  
  if (row$commuting_score >= 80) {
    reasons <- c(reasons, "strong commuting fit")
  }
  
  if (row$touring_score >= 80) {
    reasons <- c(reasons, "good touring capability")
  }
  
  if (row$performance_score >= 70) {
    reasons <- c(reasons, "strong performance profile")
  }
  
  if (row$quality_score >= 70) {
    reasons <- c(reasons, "solid rating history")
  }
  
  if (row$value_score >= 70) {
    reasons <- c(reasons, "strong value score")
  }
  
  if (length(reasons) == 0) {
    reasons <- "balanced overall fit"
  }
  
  paste(
    "Recommended because it has",
    paste(reasons, collapse = ", "),
    "."
  )
}

# ========================================================
# 6. Main Recommendation Function
# ========================================================

recommend_motorcycles <- function(
    moto_model,
    experience = "Beginner",
    rider_height = 68,
    use_case = "Commuting",
    hp_min = 30,
    hp_max = 80,
    weight_min = 300,
    weight_max = 500,
    top_n = 10,
    max_per_brand = 2
) {
  
  df <- moto_model %>%
    filter(
      horsepower >= hp_min,
      horsepower <= hp_max,
      weight_lbs >= weight_min,
      weight_lbs <= weight_max,
      Year >= 2015,
      category_group != "ATV",
      category_group != "Dirt / Mini",
      category_group != "Other"
    )
  
  if (experience == "Beginner") {
    df <- df %>%
      filter(
        horsepower <= 85,
        weight_lbs <= 550
      )
  }
  
  if (use_case == "Performance") {
    df <- df %>%
      filter(
        category_group %in% c("Sport", "Standard / Naked")
      )
  }
  
  if (use_case == "Touring") {
    df <- df %>%
      filter(
        category_group %in% c(
          "Touring",
          "Adventure / Dual Sport",
          "Cruiser",
          "Sport"
        ),
        fuel_capacity_gal >= 3.5
      )
  }
  
  if (use_case == "Adventure") {
    df <- df %>%
      filter(
        category_group %in% c(
          "Adventure / Dual Sport",
          "Touring",
          "Standard / Naked"
        )
      )
  }
  
  if (use_case == "Commuting") {
    df <- df %>%
      filter(
        category_group %in% c(
          "Standard / Naked",
          "Scooter",
          "Sport",
          "Cruiser"
        )
      )
  }
  
  if (nrow(df) == 0) {
    return(
      tibble(
        message = "No motorcycles matched those constraints. Try widening horsepower or weight range."
      )
    )
  }
  
  hp_mid <- mean(c(hp_min, hp_max))
  weight_mid <- mean(c(weight_min, weight_max))
  ideal_seat <- rider_height * 0.45
  
  df <- df %>%
    mutate(
      hp_range_fit = case_when(
        horsepower >= hp_min & horsepower <= hp_max ~ 100,
        TRUE ~ 0
      ),
      
      weight_range_fit = case_when(
        weight_lbs >= weight_min & weight_lbs <= weight_max ~ 100,
        TRUE ~ 0
      ),
      
      hp_center_fit = pmax(
        0,
        100 - abs(horsepower - hp_mid) / (hp_max - hp_min) * 100
      ),
      
      weight_center_fit = pmax(
        0,
        100 - abs(weight_lbs - weight_mid) / (weight_max - weight_min) * 100
      ),
      
      ergonomics_fit = pmax(
        0,
        100 - abs(seat_height_in - ideal_seat) * 10
      ),
      
      experience_fit = case_when(
        experience == "Beginner" & horsepower <= 50 ~ 100,
        experience == "Beginner" & horsepower <= 75 ~ 80,
        experience == "Beginner" & horsepower <= 85 ~ 60,
        experience == "Beginner" ~ 20,
        
        experience == "Intermediate" & horsepower <= 120 ~ 100,
        experience == "Intermediate" & horsepower <= 150 ~ 80,
        experience == "Intermediate" ~ 60,
        
        experience == "Advanced" ~ 100,
        TRUE ~ 70
      ),
      
      use_case_fit = case_when(
        use_case == "Commuting" ~ commuting_score,
        use_case == "Touring" ~ touring_score,
        use_case == "Performance" ~ performance_score,
        use_case == "Adventure" & category_group == "Adventure / Dual Sport" ~ 100,
        use_case == "Adventure" ~ touring_score,
        use_case == "Weekend Fun" ~ pmax(performance_score, commuting_score),
        TRUE ~ 60
      ),
      
      utility_score =
        0.25 * use_case_fit +
        0.20 * experience_fit +
        0.15 * ergonomics_fit +
        0.15 * hp_center_fit +
        0.10 * weight_center_fit +
        0.05 * quality_score +
        0.05 * value_score +
        0.05 * fuel_capacity_gal * 10
    )
  
  # Diversity: limit how many recommendations come from one brand
  results <- df %>%
    arrange(desc(utility_score)) %>%
    group_by(Brand) %>%
    slice_head(n = max_per_brand) %>%
    ungroup() %>%
    arrange(desc(utility_score)) %>%
    slice_head(n = top_n) %>%
    rowwise() %>%
    mutate(
      explanation = create_explanation(cur_data())
    ) %>%
    ungroup() %>%
    mutate(
      final_score = utility_score
    ) %>%
    select(
      Brand,
      Model,
      Year,
      category_group,
      horsepower,
      weight_lbs,
      seat_height_in,
      fuel_capacity_gal,
      quality_score,
      value_score,
      beginner_score,
      performance_score,
      touring_score,
      commuting_score,
      cluster,
      utility_score,
      final_score,
      explanation
    )
  
  results
}

# ========================================================
# 7. Example Test
# ========================================================
# Run this after sourcing the file and loading moto_model.
#
# source("R/recommender.R")
# moto_model <- read_csv("data/clean/moto_model.csv", show_col_types = FALSE)
#
# recommend_motorcycles(
#   moto_model = moto_model,
#   experience = "Beginner",
#   rider_height = 68,
#   use_case = "Commuting",
#   desired_hp = 50,
#   desired_weight = 425,
#   top_n = 10
# )