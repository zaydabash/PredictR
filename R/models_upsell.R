#' Upsell Modeling Functions
#'
#' @description
#' Functions to train and predict upsell likelihood using tidymodels.

library(dplyr)
library(tidymodels)

#' Train Upsell Model
#'
#' @param features Data frame with features.
#' @param split_ratio Train/test split ratio.
#' @param tune Logical, whether to tune hyperparameters.
#' @return A list with the model workflow and test data.
train_upsell_model <- function(features, split_ratio = 0.8, tune = FALSE) {
  
  # Define Target: "High Value Potential"
  features <- features %>%
    mutate(is_premium = as.factor(ifelse(plan_tier %in% c("Pro", "Enterprise"), "Yes", "No")))
  
  features[is.na(features)] <- 0
  
  set.seed(123)
  split <- initial_split(features, prop = split_ratio, strata = is_premium)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Recipe
  rec <- recipe(is_premium ~ ., data = train_data) %>%
    update_role(account_id, new_role = "ID") %>%
    step_rm(plan_tier, current_mrr, region) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  # Model Specification
  if (tune) {
    spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>%
      set_engine("ranger", importance = "impurity") %>%
      set_mode("classification")
    
    wf <- workflow() %>%
      add_recipe(rec) %>%
      add_model(spec)
    
    # Tuning Grid
    grid <- grid_regular(mtry(range = c(1, 5)), min_n(), levels = 3)
    folds <- vfold_cv(train_data, v = 5)
    
    res <- tune_grid(
      wf,
      resamples = folds,
      grid = grid,
      metrics = metric_set(roc_auc)
    )
    
    best_params <- select_best(res, metric = "roc_auc")
    wf <- finalize_workflow(wf, best_params)
    
  } else {
    spec <- rand_forest(trees = 100) %>%
      set_engine("ranger", importance = "impurity") %>%
      set_mode("classification")
    
    wf <- workflow() %>%
      add_recipe(rec) %>%
      add_model(spec)
  }
  
  # Fit
  fit <- fit(wf, data = train_data)
  
  list(model = fit, test_data = test_data)
}

#' Predict Upsell
#'
#' @param model Fitted workflow object.
#' @param new_data Data frame to predict on.
#' @return Vector of probabilities for "Yes".
predict_upsell <- function(model, new_data) {
  new_data[is.na(new_data)] <- 0
  predict(model, new_data, type = "prob")$.pred_Yes
}
