#' Data Preparation Functions
#'
#' @description
#' Functions to load, validate, and clean data before ingestion.

library(readr)
library(dplyr)
library(lubridate)

#' Load and Clean Data
#'
#' @param events_path Path to events CSV.
#' @param accounts_path Path to accounts CSV.
#' @param subs_path Path to subscriptions CSV.
#' @return A list containing cleaned data frames.
load_and_clean_data <- function(events_path, accounts_path, subs_path) {
  
  # Load Data
  events <- read_csv(events_path, show_col_types = FALSE)
  accounts <- read_csv(accounts_path, show_col_types = FALSE)
  subs <- read_csv(subs_path, show_col_types = FALSE)
  
  # Clean Accounts
  accounts <- accounts %>%
    mutate(
      signup_date = as.Date(signup_date),
      region = as.factor(region)
    )
  
  # Clean Subscriptions
  subs <- subs %>%
    mutate(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      plan_tier = as.factor(plan_tier),
      status = as.factor(status)
    )
  
  # Clean Events
  events <- events %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
      event_type = as.factor(event_type)
    )
  
  list(events = events, accounts = accounts, subscriptions = subs)
}

#' Validate Schema
#'
#' @param df Data frame to check.
#' @param required_cols Vector of required column names.
#' @return Logical TRUE if valid, stops with error otherwise.
validate_schema <- function(df, required_cols) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }
  TRUE
}

#' Ingest Data into DB
#'
#' @param conn DBI connection.
#' @param data_list List of data frames (events, accounts, subscriptions).
#' @return NULL
ingest_data <- function(conn, data_list) {
  write_table(conn, "accounts", data_list$accounts, overwrite = TRUE)
  write_table(conn, "subscriptions", data_list$subscriptions, overwrite = TRUE)
  write_table(conn, "events", data_list$events, overwrite = TRUE)
  message("Data ingestion complete.")
}
