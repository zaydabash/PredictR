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
      signup_date = as.Date(parse_date_time(signup_date, orders = c("ymd", "mdy", "dmy"))),
      region = as.factor(region)
    )
  
  # Clean Subscriptions
  subs <- subs %>%
    mutate(
      start_date = as.Date(parse_date_time(start_date, orders = c("ymd", "mdy", "dmy"))),
      end_date = as.Date(parse_date_time(end_date, orders = c("ymd", "mdy", "dmy"))),
      plan_tier = as.factor(plan_tier),
      status = as.factor(status)
    )
  
  # Clean Events
  events <- events %>%
    mutate(
      timestamp = parse_date_time(timestamp, orders = c("ymd HMS", "mdy HMS", "dmy HMS")),
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
#' @param tenant_id Optional identifier for data isolation.
#' @return NULL
ingest_data <- function(conn, data_list, tenant_id = NULL) {
  # Define required schemas
  schemas <- list(
    accounts = c("account_id", "name", "signup_date", "region"),
    subscriptions = c("subscription_id", "account_id", "plan_tier", "mrr", "status", "start_date"),
    events = c("event_id", "account_id", "event_type", "timestamp")
  )
  
  # Validate all
  validate_schema(data_list$accounts, schemas$accounts)
  validate_schema(data_list$subscriptions, schemas$subscriptions)
  validate_schema(data_list$events, schemas$events)
  
  # Atomic transaction
  dbBegin(conn)
  tryCatch({
    write_table(conn, "accounts", data_list$accounts, overwrite = TRUE, tenant_id = tenant_id)
    write_table(conn, "subscriptions", data_list$subscriptions, overwrite = TRUE, tenant_id = tenant_id)
    write_table(conn, "events", data_list$events, overwrite = TRUE, tenant_id = tenant_id)
    dbCommit(conn)
    message("Data ingestion complete.")
  }, error = function(e) {
    dbRollback(conn)
    stop(paste("Ingestion failed, transaction rolled back:", e$message))
  })
}
