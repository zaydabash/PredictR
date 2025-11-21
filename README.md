# PredictR

**PredictR** is a lightweight, self-contained predictive analytics template for SaaS businesses. It allows users to upload their data (events, accounts, subscriptions), builds churn and upsell models on the fly, and visualizes key metrics in an interactive dashboard.

Built with **R**, **Shiny**, **SQLite**, and **tidymodels** (via caret).

## Features

- **Data Ingestion**: Upload CSVs and automatically persist them to a local SQLite database.
- **Automated Feature Engineering**: Calculates account tenure, MRR, and recent activity metrics.
- **Churn Prediction**: Trains a logistic regression model to identify high-risk accounts.
- **Upsell Modeling**: Identifies accounts with high usage potential for plan upgrades.
- **Revenue Dashboard**: Visualizes MRR distribution and plan tiers.

## Project Structure

- `app/`: Contains the Shiny application (`ui.R`, `server.R`, `global.R`).
- `R/`: Core logic libraries.
  - `data_prep.R`: Data cleaning and validation.
  - `feature_engineering.R`: Feature creation pipeline.
  - `models_*.R`: Model training and prediction logic.
  - `metrics.R`: Evaluation metrics and plotting.
  - `db.R`: Database helper functions.
- `data/sample/`: Synthetic sample data to get started quickly.
- `sql/`: Database schema definitions.
- `deploy/`: Docker configuration for deployment.

## How to Run

### Option 1: Run Locally (R)

Requires R and the following packages: `shiny`, `dplyr`, `ggplot2`, `DT`, `DBI`, `RSQLite`, `readr`, `tidyr`, `caret`, `pROC`, `e1071`.

1. Clone the repository.
2. Open R or RStudio in the project root.
3. Run the app:
   ```r
   shiny::runApp("app")
   ```

### Option 2: Run via Docker

1. Build and run the container using the helper script:
   ```bash
   ./deploy/run.sh
   ```
2. Open your browser to [http://localhost:3838](http://localhost:3838).

## Workflow

1. **Load Data**: Go to the "Data" tab. You can upload your own CSVs or click "Load Sample Data" to see the app in action.
2. **Train Models**: Navigate to "Churn Model" or "Upsell Model" and click "Train Model".
3. **Analyze**: View the ROC curves, metrics, and the list of high-risk/high-opportunity accounts.
