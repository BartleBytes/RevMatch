# RevMatch: Motorcycle Space Explorer

## Overview

RevMatch is an interactive motorcycle recommendation and exploration platform that combines recommendation systems, feature engineering, clustering, embeddings, and interactive visualization to help riders discover motorcycles that fit their preferences and riding goals.

Unlike traditional recommendation systems that simply return a ranked list of motorcycles, RevMatch creates a **3D Motorcycle Space** where every motorcycle is represented as a point in a latent feature space. Users can explore this space, identify motorcycle archetypes, discover similar bikes, and visualize where their ideal motorcycle lies within the broader motorcycle universe.

---

## Project Objectives

The primary goals of RevMatch are:

* Build a motorcycle recommendation engine using engineered motorcycle attributes.
* Create a latent-space representation of motorcycles using dimensionality reduction techniques.
* Visualize motorcycle relationships in an interactive 3D environment.
* Recommend motorcycles using cosine similarity and utility optimization.
* Enable bike-to-bike and rider-to-bike exploration.
* Demonstrate modern recommendation system concepts in an intuitive visual format.

---

## Key Features

### Motorcycle Embedding Space

Each motorcycle is transformed into a feature vector containing characteristics such as:

* Horsepower
* Torque
* Weight
* Seat height
* Fuel capacity
* Beginner friendliness
* Performance capability
* Touring suitability
* Commuting suitability
* Quality score
* Value score

These features are standardized and projected into a lower-dimensional embedding space where motorcycles with similar characteristics naturally cluster together.

---

### Interactive 3D Visualization

The application allows users to:

* Explore the entire motorcycle landscape.
* Rotate and zoom through motorcycle space.
* View motorcycle clusters and archetypes.
* Select motorcycles and inspect nearby alternatives.
* Visualize recommendation results directly within the embedding space.

---

### Rider Profile Engine

Users can define:

* Experience level
* Riding goals
* Rider height
* Desired horsepower range
* Desired weight range

These preferences are converted into a synthetic rider vector that can be compared against every motorcycle in the database.

---

### Cosine Similarity Recommendations

Recommendations are generated using cosine similarity between:

* User preference vectors
* Motorcycle feature vectors

This approach identifies motorcycles that are most aligned with a rider's desired characteristics.

---

### Motorcycle Archetypes

K-Means clustering is used to identify motorcycle archetypes such as:

* Beginner-friendly standards
* Lightweight commuters
* High-performance sport bikes
* Touring motorcycles
* Adventure motorcycles
* Cruisers

These clusters provide a higher-level understanding of motorcycle design patterns.

---

## Data Pipeline

### Data Source

Motorcycle specifications are sourced from a curated motorcycle dataset containing:

* Manufacturer
* Model
* Year
* Category
* Engine specifications
* Dimensions
* Fuel capacity
* User ratings

---

### Feature Engineering

Raw motorcycle specifications are transformed into engineered features including:

#### Beginner Score

Measures how approachable a motorcycle is for new riders.

Factors include:

* Horsepower
* Weight

#### Performance Score

Measures acceleration and sporting capability.

Factors include:

* Power-to-weight ratio

#### Touring Score

Measures long-distance suitability.

Factors include:

* Category
* Fuel capacity

#### Commuting Score

Measures practicality for daily transportation.

Factors include:

* Weight
* Motorcycle category

#### Quality Score

Derived from rider ratings.

#### Value Score

Derived from horsepower-per-displacement efficiency.

---

## Machine Learning Components

### Feature Scaling

All recommendation features are standardized prior to clustering and dimensionality reduction.

### K-Means Clustering

Motorcycles are grouped into archetypal clusters.

### Principal Component Analysis (PCA)

Used to create lower-dimensional visual representations of motorcycle relationships.

### Cosine Similarity

Used for:

* Rider-to-bike recommendations
* Bike-to-bike similarity search

---

## Project Structure

```text
RevMatch/
├── app.R
│
├── R/
│   ├── recommender.R
│   └── recommender_space.R
│
├── data/
│   ├── raw/
│   │   └── all_bikez_curated.csv
│   │
│   └── clean/
│       ├── moto_model.csv
│       ├── moto_pca.csv
│       └── moto_catalog_major_recent.csv
│
├── scripts/
│   └── DataEngineering.R
│
├── models/
│   ├── kmeans_fit.rds
│   └── pca_fit.rds
│
└── README.md
```

---

## Technology Stack

### Programming Language

* R

### Frameworks

* Shiny
* bslib

### Visualization

* Plotly
* ggplot2

### Data Processing

* tidyverse
* dplyr
* readr
* tidyr

### Machine Learning

* PCA
* K-Means Clustering
* Cosine Similarity

---

## Future Enhancements

Potential future improvements include:

### UMAP Embeddings

Replace PCA with UMAP to better preserve local motorcycle relationships.

### Ownership Journey Modeling

Predict likely upgrade paths such as:

```text
Ninja 400
    ↓
MT-07
    ↓
Street Triple
```

### Rider Archetypes

Identify rider personas such as:

* Explorer
* Canyon Rider
* Commuter
* Collector
* Tourer

### Market Data Integration

Incorporate:

* MSRP
* Used market prices
* Insurance estimates
* Reliability metrics

### Motorcycle Universe Explorer

Expand the application into a full interactive motorcycle knowledge graph connecting:

* Motorcycles
* Manufacturers
* Categories
* Rider archetypes
* Ownership paths

---

## Educational Value

RevMatch demonstrates a variety of analytics and machine learning concepts including:

* Recommendation systems
* Feature engineering
* Dimensionality reduction
* Similarity search
* Clustering
* Interactive visualization
* User preference modeling
* Human-centered decision support

The project serves as an example of how recommendation systems can move beyond ranked lists and become interactive environments for exploration and discovery.

---

## Author

Dylan Bartle

MS in Business Analytics Candidate
University of Colorado Denver

Business Intelligence Analyst
Partner Colorado Credit Union
