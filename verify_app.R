# Verification Script

# Set working directory to app/ so relative paths work
setwd("app")

message("Sourcing global.R...")
tryCatch({
  source("global.R")
  message("global.R sourced successfully.")
}, error = function(e) {
  stop("Error sourcing global.R: ", e$message)
})

message("Checking functions...")
if (exists("load_and_clean_data") && exists("train_churn_model")) {
  message("Core functions found.")
} else {
  stop("Core functions missing.")
}

message("Checking dependencies...")
required_pkgs <- c("tidymodels", "plotly", "testthat", "yardstick")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  warning("Missing packages: ", paste(missing_pkgs, collapse = ", "))
} else {
  message("All dependencies found.")
}

message("Checking DB...")
if (file.exists("../predictR.sqlite")) {
  message("DB file created.")
} else {
  message("DB file not found (expected if not run yet, but global.R should have init it).")
}

message("Verification complete.")
