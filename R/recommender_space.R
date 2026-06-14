
# ========================================================
# RevMatch Motorcycle Space Engine
# Purpose: Build a 3D motorcycle embedding space and
# recommend bikes with cosine similarity.
# ========================================================

library(tidyverse)
library(scales)

# -----------------------------
# Feature Columns
# -----------------------------
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

# -----------------------------
# Cosine Similarity
# -----------------------------
cosine_similarity_vec <- function(a, b) {
  denom <- sqrt(sum(a^2)) * sqrt(sum(b^2))

  if (denom == 0 || is.na(denom)) {
    return(NA_real_)
  }

  sum(a * b) / denom
}

# -----------------------------
# Cluster Labels
# -----------------------------
label_motorcycle_clusters <- function(df) {
  cluster_profiles <- df %>%
    group_by(cluster) %>%
    summarise(
      avg_hp = mean(horsepower, na.rm = TRUE),
      avg_weight = mean(weight_lbs, na.rm = TRUE),
      avg_beginner = mean(beginner_score, na.rm = TRUE),
      avg_performance = mean(performance_score, na.rm = TRUE),
      avg_touring = mean(touring_score, na.rm = TRUE),
      avg_commuting = mean(commuting_score, na.rm = TRUE),
      top_category = names(sort(table(category_group), decreasing = TRUE))[1],
      .groups = "drop"
    ) %>%
    mutate(
      cluster_label = case_when(
        avg_beginner >= 75 & avg_weight <= 450 ~ "Accessible starters",
        avg_performance >= 70 & avg_hp >= 100 ~ "High-performance machines",
        avg_touring >= 75 ~ "Long-distance explorers",
        avg_commuting >= 75 ~ "Urban commuters",
        top_category == "Cruiser" ~ "Cruiser comfort zone",
        top_category == "Adventure / Dual Sport" ~ "Adventure crossover",
        top_category == "Sport" ~ "Sport-focused middleweights",
        TRUE ~ paste("Design cluster", cluster)
      )
    ) %>%
    select(cluster, cluster_label)

  df %>%
    left_join(cluster_profiles, by = "cluster")
}

# -----------------------------
# Build 3D Motorcycle Space
# -----------------------------
build_motorcycle_space <- function(moto_catalog) {
  missing_cols <- setdiff(feature_cols, names(moto_catalog))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required feature columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  df <- moto_catalog %>%
    filter(
      if_all(all_of(feature_cols), ~ !is.na(.x) & is.finite(.x)),
      category_group != "ATV",
      category_group != "Dirt / Mini",
      category_group != "Other"
    ) %>%
    mutate(
      bike_id = paste(Brand, Model, Year, row_number(), sep = "_"),
      bike_label = paste(Year, Brand, Model),
      cluster = factor(cluster)
    )

  feature_scaled <- df %>%
    select(all_of(feature_cols)) %>%
    scale()

  pca_fit <- prcomp(feature_scaled, center = FALSE, scale. = FALSE)

  embedded <- df %>%
    mutate(
      Embed1 = pca_fit$x[, 1],
      Embed2 = pca_fit$x[, 2],
      Embed3 = pca_fit$x[, 3]
    )

  # Store scaled feature values for cosine calculations.
  scaled_features <- as_tibble(feature_scaled)
  names(scaled_features) <- paste0("scaled_", feature_cols)

  embedded <- bind_cols(embedded, scaled_features)

  label_motorcycle_clusters(embedded)
}

# -----------------------------
# Build User Vector
# -----------------------------
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
    use_case == "Adventure" ~ 85,
    use_case == "Weekend Fun" ~ 55,
    TRUE ~ 40
  )

  commuting_score <- case_when(
    use_case == "Commuting" ~ 100,
    use_case == "Weekend Fun" ~ 75,
    use_case == "Adventure" ~ 55,
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
    quality_score = 72,
    value_score = 70
  )
}

# -----------------------------
# Explanation Engine
# -----------------------------
create_space_explanation <- function(row) {
  reasons <- c()

  if (row$beginner_score >= 80) {
    reasons <- c(reasons, "approachable for newer riders")
  }

  if (row$performance_score >= 70) {
    reasons <- c(reasons, "sporty relative to its weight")
  }

  if (row$touring_score >= 80) {
    reasons <- c(reasons, "strong long-distance capability")
  }

  if (row$commuting_score >= 80) {
    reasons <- c(reasons, "practical for daily riding")
  }

  if (row$seat_height_in <= 31.5) {
    reasons <- c(reasons, "manageable seat height")
  }

  if (row$fuel_capacity_gal >= 4.5) {
    reasons <- c(reasons, "useful fuel range")
  }

  if (length(reasons) == 0) {
    reasons <- "balanced design profile"
  }

  paste("Near your rider vector because it has", paste(reasons, collapse = ", "), ".")
}

# -----------------------------
# Recommend by Embedding Similarity
# -----------------------------
recommend_by_embedding <- function(
    space_df,
    user_vector,
    top_n = 10
) {
  # Scale user vector using the same center/scale implied by the visible data.
  centers <- space_df %>%
    summarise(across(all_of(feature_cols), mean, na.rm = TRUE))

  sds <- space_df %>%
    summarise(across(all_of(feature_cols), sd, na.rm = TRUE))

  user_scaled <- map_dbl(feature_cols, function(col) {
    (user_vector[[col]][1] - centers[[col]][1]) / sds[[col]][1]
  })

  scaled_cols <- paste0("scaled_", feature_cols)

  space_df %>%
    rowwise() %>%
    mutate(
      cosine_similarity = cosine_similarity_vec(
        c_across(all_of(scaled_cols)),
        user_scaled
      ),
      fit_score = rescale(cosine_similarity, to = c(0, 100), from = c(-1, 1)),
      explanation = create_space_explanation(cur_data())
    ) %>%
    ungroup() %>%
    arrange(desc(cosine_similarity)) %>%
    slice_head(n = top_n)
}

# -----------------------------
# Find Similar Bikes
# -----------------------------
find_similar_bikes <- function(
    space_df,
    bike_id_value,
    top_n = 10
) {
  scaled_cols <- paste0("scaled_", feature_cols)

  selected <- space_df %>%
    filter(bike_id == bike_id_value) %>%
    slice(1)

  if (nrow(selected) == 0) {
    return(tibble())
  }

  selected_vec <- selected %>%
    select(all_of(scaled_cols)) %>%
    as.numeric()

  space_df %>%
    filter(bike_id != bike_id_value) %>%
    rowwise() %>%
    mutate(
      cosine_similarity = cosine_similarity_vec(
        c_across(all_of(scaled_cols)),
        selected_vec
      )
    ) %>%
    ungroup() %>%
    arrange(desc(cosine_similarity)) %>%
    slice_head(n = top_n)
}
