# R/cleaning.R

library(tidyverse)
library(tidytext)

# 1. Load data ------------------------------------------------------------

load_off_data <- function(path = "openfoodfacts_api_sample.rds") {
  off <- readRDS(path)
  as_tibble(off)
}

# 2. Basic cleaning -------------------------------------------------------

clean_off_products <- function(off) {
  off %>%
    filter(!is.na(nova_group)) %>%
    filter(!is.na(ingredients_text)) %>%
    mutate(
      # cleaned ingredient text
      ingredients_clean = ingredients_text %>%
        str_to_lower() %>%
        str_replace_all("[^a-z ]", " ") %>%
        str_squish(),
      # numeric nutrient columns (already flattened in this dataset)
      energy_kj   = suppressWarnings(as.numeric(`nutriments.energy`)),
      energy_kcal = suppressWarnings(as.numeric(`nutriments.energy-kcal`))
    )
}

# 3. Tokenization ---------------------------------------------------------

tokenize_ingredients <- function(cleaned_df) {
  cleaned_df %>%
    select(product_name, nova_group, ingredients_clean) %>%
    unnest_tokens(word, ingredients_clean)
}

# 4. Token cleaning (stopwords, junk, very short words) -------------------

custom_stopwords <- tibble::tibble(
  word = c(
    "de", "d", "du", "des", "en", "et", "a", "au", "aux",
    "avec", "pour", "sur", "dans", "par",
    "le", "la", "les"
  )
)

clean_tokens <- function(tokens_df) {
  tokens_df %>%
    # keep only alphabetic tokens
    filter(str_detect(word, "^[a-z]+$")) %>%
    # drop very short tokens
    filter(nchar(word) >= 3) %>%
    # remove english stopwords
    anti_join(stop_words, by = "word") %>%
    # remove custom connector/french stopwords
    anti_join(custom_stopwords, by = "word")
}

# 5. Top words and document-term matrix -----------------------------------

get_top_words <- function(tokens_clean_df, n = 50) {
  tokens_clean_df %>%
    count(word, sort = TRUE) %>%
    slice_max(n, n = n) %>%
    pull(word)
}

build_dtm <- function(tokens_clean_df, top_words) {
  tokens_clean_df %>%
    filter(word %in% top_words) %>%
    count(product_name, word) %>%
    tidyr::pivot_wider(
      names_from = word,
      values_from = n,
      values_fill = 0
    )
}

# 6. Build modeling dataset -----------------------------------------------

build_model_data <- function(cleaned_df, dtm_df) {
  cleaned_df %>%
    select(product_name, nova_group, energy_kcal) %>%
    inner_join(dtm_df, by = "product_name") %>%
    mutate(
      is_upf = if_else(nova_group == 4, 1L, 0L)
    )
}
