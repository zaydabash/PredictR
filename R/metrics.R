#' Metrics and Plotting Functions
#'
#' @description
#' Helper functions for model evaluation using yardstick.

library(ggplot2)
library(pROC)
library(yardstick)

#' Calculate Metrics
#'
#' @param predictions Numeric vector of probabilities.
#' @param truth Factor vector of true labels (reference level first).
#' @return Data frame of metrics.
calculate_metrics <- function(predictions, truth) {
  # Create a tibble for yardstick
  data <- tibble(
    truth = truth,
    pred = predictions,
    estimate = as.factor(ifelse(predictions > 0.5, levels(truth)[2], levels(truth)[1]))
  )
  
  # Ensure levels match
  levels(data$estimate) <- levels(truth)
  
  # Metrics
  acc <- accuracy(data, truth, estimate)$.estimate
  sens <- sensitivity(data, truth, estimate)$.estimate
  spec <- specificity(data, truth, estimate)$.estimate
  roc <- roc_auc(data, truth, pred)$.estimate
  pr_auc_val <- pr_auc(data, truth, pred)$.estimate
  
  tibble(
    Metric = c("Accuracy", "Sensitivity (Recall)", "Specificity", "ROC AUC", "PR AUC"),
    Value = c(acc, sens, spec, roc, pr_auc_val)
  )
}

#' Plot Precision-Recall Curve
#'
#' @param predictions Numeric vector of probabilities.
#' @param truth Factor vector of true labels.
#' @return A ggplot object.
plot_pr_curve <- function(predictions, truth) {
  data <- tibble(truth = truth, pred = predictions)
  
  pr_curve(data, truth, pred) %>%
    autoplot() +
    theme_minimal() +
    ggtitle("Precision-Recall Curve")
}

#' Plot Lift Chart
#'
#' @param predictions Numeric vector of probabilities.
#' @param truth Factor vector of true labels.
#' @return A ggplot object.
plot_lift_chart <- function(predictions, truth) {
  data <- tibble(truth = truth, pred = predictions)
  
  lift_curve(data, truth, pred) %>%
    autoplot() +
    theme_minimal() +
    ggtitle("Lift Chart")
}

#' Plot ROC Curve
#'
#' @param predictions Numeric vector of probabilities.
#' @param truth Factor vector of true labels.
#' @return A ggplot object.
plot_roc_curve <- function(predictions, truth) {
  roc_obj <- roc(as.numeric(truth), predictions, quiet = TRUE)
  
  ggroc(roc_obj) +
    theme_minimal() +
    ggtitle(paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")")) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey")
}
