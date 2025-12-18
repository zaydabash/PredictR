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
#' @param tune Logical, whether to tune hyperparameters.
#' @return A list with the model workflow and test data.
train_churn_model <- function(features, split_ratio = 0.8, tune = FALSE) {
  
  # Ensure target is a factor with descriptive levels for tidymodels
  features <- features %>%
    mutate(is_churned = factor(is_churned, levels = c("0", "1"), labels = c("No", "Yes")))
  
  # Split
  set.seed(123)
  split <- initial_split(features, prop = split_ratio, strata = is_churned)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Recipe with robust preprocessing
  rec <- recipe(is_churned ~ ., data = train_data) %>%
    update_role(account_id, new_role = "ID") %>%
    step_rm(has_role("ID")) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  # Handling class imbalance (using downsampling for simplicity in this template)
  # requires 'themis' package, but let's check if we can doing it via weights or simple sampling
  # For now, let's use a standard model and suggest smote/downsampling in advanced docs
  
  # Model Specification
  if (tune) {
    spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
      set_engine("glmnet") %>%
      set_mode("classification")
    
    wf <- workflow() %>%
      add_recipe(rec) %>%
      add_model(spec)
    
    # Tuning Grid
    grid <- grid_regular(penalty(), mixture(), levels = 5)
    folds <- vfold_cv(train_data, v = 5, strata = is_churned)
    
    res <- tune_grid(
      wf,
      resamples = folds,
      grid = grid,
      metrics = metric_set(roc_auc, pr_auc)
    )
    
    best_params <- select_best(res, metric = "pr_auc") # PR AUC better for imbalanced churn
    wf <- finalize_workflow(wf, best_params)
    
  } else {
    spec <- logistic_reg() %>%
      set_engine("glm") %>%
      set_mode("classification")
    
    wf <- workflow() %>%
      add_recipe(rec) %>%
      add_model(spec)
  }
  
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
  predict(model, new_data, type = "prob")$.pred_Yes
}
