# Global Setup

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(DBI)
library(RSQLite)
library(readr)
library(tidyr)
library(tidymodels)
library(pROC)
library(plotly)

# Source helper functions
source("../R/db.R")
source("../R/data_prep.R")
source("../R/feature_engineering.R")
source("../R/models_churn.R")
source("../R/models_upsell.R")
source("../R/metrics.R")

# Initialize DB if needed
db_path <- "../predictR.sqlite"
if (!file.exists(db_path)) {
  init_db(db_path, "../sql/schema.sql")
}

# Global DB connection (managed per session usually, but for simple app we can open/close in server)
# Better: use a pool or open/close in reactive context. We'll open/close in server functions.
