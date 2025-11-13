# R/model.R

library(tidyverse)

# 1. Train/test split -----------------------------------------------------

split_train_test <- function(model_data, prop = 0.8, seed = 123) {
  set.seed(seed)
  n <- nrow(model_data)
  train_idx <- sample(seq_len(n), size = floor(prop * n))
  
  list(
    train = model_data[train_idx, ],
    test  = model_data[-train_idx, ]
  )
}

# 2. Fit logistic regression model ---------------------------------------

fit_logistic_upf <- function(train_df) {
  feature_cols <- setdiff(names(train_df), c("product_name", "nova_group", "is_upf"))
  
  formula_upf <- as.formula(
    paste("is_upf ~", paste(feature_cols, collapse = " + "))
  )
  
  glm(
    formula_upf,
    data   = train_df,
    family = binomial()
  )
}

# 3. Evaluate model -------------------------------------------------------

evaluate_upf_model <- function(fit, test_df) {
  test_df <- test_df %>%
    mutate(
      prob = predict(fit, newdata = test_df, type = "response"),
      pred = if_else(prob > 0.5, 1L, 0L)
    )
  
  cm <- table(
    truth = test_df$is_upf,
    pred  = test_df$pred
  )
  
  accuracy <- mean(test_df$pred == test_df$is_upf, na.rm = TRUE)
  
  list(
    test_data = test_df,
    confusion = cm,
    accuracy  = accuracy
  )
}
