#' Churn Modeling Functions
#'
#' @description
#' Functions to train and predict churn using tidymodels.

library(dplyr)
library(tidymodels)

#' Train Churn Model
#'
#' @param features Data frame with features and target 'is_churned'.
#' @param split_ratio Train/test split ratio.
#' @return A list with the model workflow and test data.
train_churn_model <- function(features, split_ratio = 0.8) {
  
  # Ensure target is a factor
  features$is_churned <- as.factor(features$is_churned)
  
  # Handle missing values (simple imputation) - better handled in recipe now
  features[is.na(features)] <- 0
  
  # Split
  set.seed(123)
  split <- initial_split(features, prop = split_ratio, strata = is_churned)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Recipe
  rec <- recipe(is_churned ~ ., data = train_data) %>%
    update_role(account_id, new_role = "ID") %>%
    step_rm(region) %>% # Remove non-numeric/categorical if not needed or handle them
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  # Model Specification
  spec <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")
  
  # Workflow
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
  
  # Fit
  fit <- fit(wf, data = train_data)
  
  list(model = fit, test_data = test_data)
}

#' Predict Churn
#'
#' @param model Fitted workflow object.
#' @param new_data Data frame to predict on.
#' @return Vector of probabilities.
predict_churn <- function(model, new_data) {
  # Handle missing values if not handled by recipe (recipe handles it if passed correctly)
  # But for safety in this simple template:
  new_data[is.na(new_data)] <- 0
  
  predict(model, new_data, type = "prob")$.pred_1
}
