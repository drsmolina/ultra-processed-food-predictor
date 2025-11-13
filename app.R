# app.R

library(shiny)
library(tidyverse)
library(tidytext)

source("cleaning.R")  # for clean_tokens() and helpers

# Load precomputed model + vocabulary
load("upf_model_objects.rda")  # loads: top_words, fit_upf

# ---- Helper to turn one ingredient string into features ----

make_feature_row <- function(ingredients_text, energy_kcal_value, top_words) {
  # Build a tiny tibble that mimics 'cleaned'
  tmp_cleaned <- tibble(
    product_name     = "user_product",
    nova_group       = NA_integer_,       # unknown for user input
    ingredients_text = ingredients_text,
    `nutriments.energy`      = NA_real_,
    `nutriments.energy-kcal` = energy_kcal_value
  ) %>%
    clean_off_products()   # reuse your cleaning logic
  
  # Tokenize + clean
  tmp_tokens <- tokenize_ingredients(tmp_cleaned)
  tmp_tokens_clean <- clean_tokens(tmp_tokens)
  
  # Build dtm for this single product, but restricted to your top_words
  tmp_dtm <- tmp_tokens_clean %>%
    filter(word %in% top_words) %>%
    count(product_name, word) %>%
    tidyr::pivot_wider(
      names_from  = word,
      values_from = n,
      values_fill = 0
    )
  
  # Ensure all top_words exist as columns, even if 0
  for (w in top_words) {
    if (!w %in% names(tmp_dtm)) {
      tmp_dtm[[w]] <- 0L
    }
  }
  
  # Ensure column order matches training data (alphabetical is fine)
  tmp_dtm <- tmp_dtm %>%
    select(product_name, sort(top_words))
  
  # Add energy_kcal to match model features
  tmp_features <- tmp_cleaned %>%
    select(product_name, energy_kcal) %>%
    inner_join(tmp_dtm, by = "product_name")
  
  tmp_features
}

# ---- UI ----

ui <- fluidPage(
  titlePanel("Ultra-Processed Food Label Checker"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "ingredients",
        "Paste ingredients list",
        placeholder = "e.g. Sugar, wheat flour, vegetable oil (palm), salt, emulsifiers (sunflower lecithin)...",
        rows = 6
      ),
      numericInput(
        "energy_kcal",
        "Energy (kcal per 100g)",
        value = NA,
        min = 0,
        step = 1
      ),
      actionButton("go", "Check product")
    ),
    
    mainPanel(
      h3("Prediction"),
      verbatimTextOutput("prediction_text"),
      h4("Details"),
      tableOutput("coef_words")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  
  # Reactive: compute features + prediction when button clicked
  result <- eventReactive(input$go, {
    req(input$ingredients)
    
    # Default energy_kcal to 0 if not provided
    energy_val <- ifelse(is.na(input$energy_kcal), 0, input$energy_kcal)
    
    # Build feature row
    new_obs <- make_feature_row(
      ingredients_text   = input$ingredients,
      energy_kcal_value  = energy_val,
      top_words          = top_words
    )
    
    # Make prediction
    prob <- predict(fit_upf, newdata = new_obs, type = "response")[1]
    pred_class <- ifelse(prob > 0.5, 1L, 0L)
    
    list(
      prob       = prob,
      pred_class = pred_class,
      new_obs    = new_obs
    )
  })
  
  # Text summary
  output$prediction_text <- renderText({
    r <- result()
    
    label <- if (r$pred_class == 1L) {
      "Likely ULTRA-PROCESSED (NOVA 4)"
    } else {
      "Likely NOT ultra-processed"
    }
    
    paste0(
      label, "\n\n",
      "Estimated probability of being ultra-processed: ",
      sprintf("%.1f%%", 100 * r$prob)
    )
  })
  
  # Simple "explanation": show which words from top_words are present
  output$coef_words <- renderTable({
    r <- result()
    obs <- r$new_obs
    
    # Which top words have non-zero counts?
    word_cols <- setdiff(names(obs), c("product_name", "energy_kcal"))
    present <- word_cols[obs[1, word_cols] > 0]
    
    tibble(
      ingredient_word = present
    )
  })
}

shinyApp(ui = ui, server = server)
