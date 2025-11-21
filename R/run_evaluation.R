# Run Evaluation Script

library(rmarkdown)
library(dplyr)
library(tidymodels)

# Source core logic
source("R/db.R")
source("R/data_prep.R")
source("R/feature_engineering.R")
source("R/models_churn.R")
source("R/models_upsell.R")

# Initialize
conn <- get_db_conn()
on.exit(DBI::dbDisconnect(conn))

if (!DBI::dbExistsTable(conn, "accounts") || DBI::dbGetQuery(conn, "SELECT count(*) as n FROM accounts")$n == 0) {
  message("Database empty. Ingesting sample data...")
  data <- load_and_clean_data(
    "data/sample/sample_events.csv",
    "data/sample/sample_accounts.csv",
    "data/sample/sample_subscriptions.csv"
  )
  ingest_data(conn, data)
}

# Load Data & Train Models (Simulated for report)
message("Loading data and training models...")
features <- create_features(conn)

# Train Churn
churn_res <- train_churn_model(features)

# Train Upsell
upsell_res <- train_upsell_model(features)

# Render Report
message("Rendering report...")
render(
  input = "R/reports/evaluation_report.Rmd",
  output_file = "evaluation_report.html",
  output_dir = "deploy",
  params = list(
    churn_model = churn_res$model,
    test_data_churn = churn_res$test_data,
    upsell_model = upsell_res$model,
    test_data_upsell = upsell_res$test_data
  )
)

message("Report generated at deploy/evaluation_report.html")
