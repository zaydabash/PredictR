#' Feature Engineering Functions
#'
#' @description
#' Functions to create features for modeling.

library(dplyr)
library(lubridate)
library(tidyr)

#' Create Features
#'
#' @param conn DBI connection.
#' @return A data frame with features for modeling.
create_features <- function(conn) {
  
  accounts <- read_table(conn, "accounts")
  subs <- read_table(conn, "subscriptions")
  events <- read_table(conn, "events")
  
  # Convert dates back to proper types (SQLite stores as string)
  accounts <- accounts %>% mutate(signup_date = as.Date(signup_date))
  subs <- subs %>% mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))
  events <- events %>% mutate(timestamp = as.POSIXct(timestamp))
  
  # 1. Account Tenure & Region
  feat_accounts <- accounts %>%
    mutate(
      account_age_days = as.numeric(Sys.Date() - signup_date)
    ) %>%
    select(account_id, region, account_age_days)
  
  # 2. Subscription Status & MRR
  # Take the most recent subscription per account
  feat_subs <- subs %>%
    group_by(account_id) %>%
    arrange(desc(start_date)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      is_churned = ifelse(status == "Churned", 1, 0),
      plan_tier = plan_tier,
      current_mrr = mrr
    ) %>%
    select(account_id, is_churned, plan_tier, current_mrr)
  
  # 3. Event Aggregates (Last 30 days)
  # For simplicity, we'll assume "today" is the max event date + 1, or just use relative to each account
  # But for a simple template, let's aggregate all history or last N days relative to a fixed reference date.
  # Let's use the max date in the dataset as "reference today".
  
  ref_date <- max(events$timestamp, na.rm = TRUE)
  
  feat_events <- events %>%
    filter(timestamp >= (ref_date - days(30))) %>%
    group_by(account_id, event_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = event_type, values_from = count, values_fill = 0, names_prefix = "n_30d_")
  
  # Join all
  features <- feat_accounts %>%
    left_join(feat_subs, by = "account_id") %>%
    left_join(feat_events, by = "account_id") %>%
    replace_na(list(n_30d_login = 0, n_30d_dashboard_view = 0, n_30d_report_export = 0)) # Fill NAs for accounts with no events
  
  # Filter out accounts with no subscription info (if any)
  features <- features %>% filter(!is.na(is_churned))
  
  return(features)
}
