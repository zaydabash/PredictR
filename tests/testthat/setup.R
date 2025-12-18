# Setup test environment
library(testthat)
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(tidymodels)

# Find project root
root <- rprojroot::find_root(rprojroot::has_dir("R"))

# Source library files (absolute paths)
r_files <- list.files(file.path(root, "R"), pattern = "^(db|data|feature|metrics|models|reports_helper).*\\.R$", full.names = TRUE)
for (f in r_files) {
  source(normalizePath(f))
}
