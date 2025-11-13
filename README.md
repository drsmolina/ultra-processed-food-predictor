# Ultra-Processed Food Score Predictor (OpenFoodFacts)

This project uses OpenFoodFacts product data to predict whether a food is ultra-processed (NOVA group 4) based on its ingredient list and basic nutrient information.

## What this notebook does

- Loads a sample of OpenFoodFacts products saved as `openfoodfacts_api_sample.rds`
- Cleans ingredient text into a machine-readable form (`ingredients_clean`)
- Tokenizes words, removes stopwords, and explores most common ingredients by NOVA group
- Computes TF–IDF to see which words are most characteristic of ultra-processed foods
- Builds a simple bag-of-words feature set (top 50 ingredient words + energy_kcal)
- Trains a logistic regression model to predict `is_upf` (NOVA 4 vs not)
- Evaluates the model on a test set (≈73% accuracy on held-out products)

## Why it matters

Ultra-processed foods are linked to chronic disease risk. Being able to classify products using only their labels is a useful building block for:

- consumer-facing health apps
- healthy basket / cart scoring systems
- public health and policy analytics

This project will also serve as the foundation for a follow-up project:
a **Healthy Basket Index**, which scores entire grocery baskets using these product-level predictions.
