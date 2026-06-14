
# ========================================================
# RevMatch Motorcycle Space Explorer
# Purpose: Interactive 3D motorcycle embedding space +
# similarity-based recommendation explorer.
# ========================================================

library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(DT)
library(scales)

source("R/recommender_space.R")

# -----------------------------
# Load Data
# -----------------------------
data_paths <- c(
  "data/clean/moto_catalog_major_recent.csv",
  "data/clean/moto_model.csv",
  "moto_catalog_major_recent.csv"
)

data_path <- data_paths[file.exists(data_paths)][1]

if (is.na(data_path)) {
  stop("Could not find motorcycle dataset. Put moto_catalog_major_recent.csv in the app folder or data/clean/.")
}

moto_catalog <- read_csv(data_path, show_col_types = FALSE)

space <- build_motorcycle_space(moto_catalog)

# -----------------------------
# UI
# -----------------------------
ui <- page_navbar(
  title = "RevMatch Motorcycle Space",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0d6efd",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  nav_panel(
    "Explore Space",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        
        h4("Build your rider vector"),
        
        selectInput(
          "experience",
          "Experience level",
          choices = c("Beginner", "Intermediate", "Advanced"),
          selected = "Beginner"
        ),
        
        selectInput(
          "use_case",
          "Primary riding goal",
          choices = c(
            "Commuting",
            "Weekend Fun",
            "Performance",
            "Touring",
            "Adventure"
          ),
          selected = "Weekend Fun"
        ),
        
        sliderInput(
          "rider_height",
          "Rider height",
          min = 60,
          max = 78,
          value = 69,
          step = 1,
          post = " in"
        ),
        
        sliderInput(
          "desired_hp",
          "Desired horsepower",
          min = 20,
          max = 180,
          value = 75,
          step = 5,
          post = " hp"
        ),
        
        sliderInput(
          "desired_weight",
          "Desired weight",
          min = 250,
          max = 700,
          value = 430,
          step = 10,
          post = " lbs"
        ),
        
        sliderInput(
          "year_min",
          "Minimum year",
          min = min(space$Year, na.rm = TRUE),
          max = max(space$Year, na.rm = TRUE),
          value = 2015,
          step = 1,
          sep = ""
        ),
        
        checkboxGroupInput(
          "categories",
          "Categories",
          choices = sort(unique(space$category_group)),
          selected = sort(unique(space$category_group))
        ),
        
        hr(),
        
        selectInput(
          "color_by",
          "Color motorcycles by",
          choices = c(
            "Category" = "category_group",
            "Cluster" = "cluster_label",
            "Beginner score" = "beginner_score",
            "Performance score" = "performance_score",
            "Touring score" = "touring_score",
            "Commuting score" = "commuting_score"
          ),
          selected = "category_group"
        ),
        
        sliderInput(
          "top_n",
          "Number of recommendations",
          min = 5,
          max = 25,
          value = 10,
          step = 1
        )
      ),
      
      card(
        card_header(
          div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            span("3D Motorcycle Embedding Space"),
            span("Click a bike to explore similar models", style = "font-size:0.9rem;color:#666;")
          )
        ),
        plotlyOutput("space_plot", height = "680px")
      )
    )
  ),
  
  nav_panel(
    "Recommendations",
    layout_columns(
      col_widths = c(5, 7),
      
      card(
        card_header("Your rider profile"),
        uiOutput("profile_card")
      ),
      
      card(
        card_header("Nearest motorcycles to your rider vector"),
        DTOutput("recommendation_table")
      )
    ),
    
    card(
      card_header("Why these bikes are near you"),
      plotlyOutput("fit_radar", height = "420px")
    )
  ),
  
  nav_panel(
    "Selected Bike",
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        card_header("Selected motorcycle"),
        uiOutput("selected_bike_card")
      ),
      
      card(
        card_header("Most similar motorcycles"),
        DTOutput("similar_table")
      )
    )
  ),
  
  nav_panel(
    "About",
    card(
      card_header("Project concept"),
      markdown(
        "
### What makes this different?

RevMatch is a **motorcycle latent-space explorer**. Every motorcycle is represented as a point in a 3D feature space based on engineered traits like horsepower, weight, seat height, fuel capacity, beginner friendliness, touring usefulness, commuting usefulness, quality, and value.

The recommendation engine works in two ways:

1. **User-to-bike similarity**  
   The app builds a rider vector from the user's preferences and finds bikes nearby using cosine similarity.

2. **Bike-to-bike similarity**  
   When the user clicks a motorcycle, the app finds its nearest neighbors in the same feature space.

This makes the app feel less like a generic product recommender and more like an interactive map of motorcycle design archetypes.
        "
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  filtered_space <- reactive({
    space %>%
      filter(
        Year >= input$year_min,
        category_group %in% input$categories
      )
  })
  
  user_vec <- reactive({
    build_user_vector(
      experience = input$experience,
      rider_height = input$rider_height,
      use_case = input$use_case,
      desired_hp = input$desired_hp,
      desired_weight = input$desired_weight
    )
  })
  
  recommendations <- reactive({
    recommend_by_embedding(
      space_df = filtered_space(),
      user_vector = user_vec(),
      top_n = input$top_n
    )
  })
  
  selected_point <- reactive({
    event_data("plotly_click", source = "moto_space")
  })
  
  selected_bike <- reactive({
    click <- selected_point()
    
    if (is.null(click) || is.null(click$key)) {
      return(NULL)
    }
    
    filtered_space() %>%
      filter(bike_id == click$key) %>%
      slice(1)
  })
  
  similar_bikes <- reactive({
    bike <- selected_bike()
    
    if (is.null(bike) || nrow(bike) == 0) {
      return(tibble())
    }
    
    find_similar_bikes(
      space_df = filtered_space(),
      bike_id_value = bike$bike_id[1],
      top_n = input$top_n
    )
  })
  
  output$space_plot <- renderPlotly({
    df <- filtered_space()
    recs <- recommendations()
    
    color_col <- input$color_by
    
    p <- plot_ly(
      data = df,
      x = ~Embed1,
      y = ~Embed2,
      z = ~Embed3,
      type = "scatter3d",
      mode = "markers",
      source = "moto_space",
      key = ~bike_id,
      color = as.formula(paste0("~", color_col)),
      marker = list(
        size = 4,
        opacity = 0.72
      ),
      text = ~paste0(
        "<b>", Brand, " ", Model, "</b><br>",
        "Year: ", Year, "<br>",
        "Category: ", category_group, "<br>",
        "HP: ", round(horsepower, 0), "<br>",
        "Weight: ", round(weight_lbs, 0), " lbs<br>",
        "Seat height: ", round(seat_height_in, 1), " in<br>",
        "Cluster: ", cluster_label
      ),
      hoverinfo = "text"
    ) %>%
      add_trace(
        data = recs,
        x = ~Embed1,
        y = ~Embed2,
        z = ~Embed3,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 8,
          symbol = "diamond",
          opacity = 0.95
        ),
        text = ~paste0(
          "<b>Recommended: ", Brand, " ", Model, "</b><br>",
          "Similarity: ", percent(cosine_similarity, accuracy = 0.1), "<br>",
          "Fit score: ", round(fit_score, 1)
        ),
        hoverinfo = "text",
        inherit = FALSE,
        name = "Your nearest bikes"
      ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Design axis 1"),
          yaxis = list(title = "Design axis 2"),
          zaxis = list(title = "Design axis 3")
        ),
        legend = list(orientation = "h"),
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
    
    p
  })
  
  output$profile_card <- renderUI({
    tags$div(
      tags$h3(paste(input$experience, input$use_case, "Rider")),
      tags$p("Your profile is converted into a synthetic motorcycle vector, then compared to every real bike using cosine similarity."),
      tags$ul(
        tags$li(paste("Height:", input$rider_height, "in")),
        tags$li(paste("Desired horsepower:", input$desired_hp, "hp")),
        tags$li(paste("Desired weight:", input$desired_weight, "lbs")),
        tags$li(paste("Primary use case:", input$use_case))
      )
    )
  })
  
  output$recommendation_table <- renderDT({
    recommendations() %>%
      transmute(
        Rank = row_number(),
        Motorcycle = paste(Year, Brand, Model),
        Category = category_group,
        `Cosine similarity` = percent(cosine_similarity, accuracy = 0.1),
        `Fit score` = round(fit_score, 1),
        HP = round(horsepower, 0),
        Weight = round(weight_lbs, 0),
        `Seat height` = round(seat_height_in, 1),
        `Why it fits` = explanation
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$selected_bike_card <- renderUI({
    bike <- selected_bike()
    
    if (is.null(bike) || nrow(bike) == 0) {
      return(tags$p("Click a motorcycle in the 3D space to inspect its nearest neighbors."))
    }
    
    tags$div(
      tags$h3(paste(bike$Year, bike$Brand, bike$Model)),
      tags$p(strong("Category: "), bike$category_group),
      tags$p(strong("Cluster: "), bike$cluster_label),
      tags$ul(
        tags$li(paste("Horsepower:", round(bike$horsepower, 0))),
        tags$li(paste("Torque:", round(bike$torque_nm, 0), "Nm")),
        tags$li(paste("Weight:", round(bike$weight_lbs, 0), "lbs")),
        tags$li(paste("Seat height:", round(bike$seat_height_in, 1), "in")),
        tags$li(paste("Fuel capacity:", round(bike$fuel_capacity_gal, 1), "gal"))
      )
    )
  })
  
  output$similar_table <- renderDT({
    bikes <- similar_bikes()
    
    if (nrow(bikes) == 0) {
      return(datatable(tibble(Message = "Click a bike in the 3D plot first."), rownames = FALSE))
    }
    
    bikes %>%
      transmute(
        Rank = row_number(),
        Motorcycle = paste(Year, Brand, Model),
        Category = category_group,
        `Similarity` = percent(cosine_similarity, accuracy = 0.1),
        HP = round(horsepower, 0),
        Weight = round(weight_lbs, 0),
        `Seat height` = round(seat_height_in, 1),
        `Design archetype` = cluster_label
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  output$fit_radar <- renderPlotly({
    recs <- recommendations() %>% slice_head(n = 5)
    
    radar_df <- recs %>%
      select(
        Motorcycle = bike_label,
        beginner_score,
        performance_score,
        touring_score,
        commuting_score,
        quality_score,
        value_score
      ) %>%
      pivot_longer(
        cols = -Motorcycle,
        names_to = "Dimension",
        values_to = "Score"
      ) %>%
      mutate(
        Dimension = recode(
          Dimension,
          beginner_score = "Beginner",
          performance_score = "Performance",
          touring_score = "Touring",
          commuting_score = "Commuting",
          quality_score = "Quality",
          value_score = "Value"
        )
      )
    
    plot_ly(
      radar_df,
      type = "scatterpolar",
      r = ~Score,
      theta = ~Dimension,
      color = ~Motorcycle,
      fill = "toself"
    ) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        showlegend = TRUE
      )
  })
}

shinyApp(ui, server)
