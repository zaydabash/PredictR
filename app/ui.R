# UI Definition

library(bslib)
library(plotly)
library(DT)

ui <- page_navbar(
  title = "PredictR | SaaS Analytics",
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#007bff",
    base_font = font_google("Inter")
  ),
  sidebar = sidebar(
    title = "Data Controls",
    fileInput("file_events", "Events CSV", accept = ".csv"),
    fileInput("file_accounts", "Accounts CSV", accept = ".csv"),
    fileInput("file_subs", "Subscriptions CSV", accept = ".csv"),
    actionButton("btn_ingest", "Ingest Data", class = "btn-primary w-100 mb-2"),
    actionButton("btn_load_sample", "Load Sample Data", class = "btn-outline-info w-100"),
    hr(),
    textOutput("status_msg")
  ),
  
  nav_panel("Dashboard",
    layout_columns(
      card(
        card_header("Plan Distribution"),
        plotlyOutput("plot_plan_dist")
      ),
      card(
        card_header("Revenue Distribution (MRR)"),
        plotlyOutput("plot_mrr_dist")
      )
    ),
    card(
      card_header("Account Summary"),
      tableOutput("table_summary")
    )
  ),
  
  nav_panel("Churn Model",
    layout_columns(
      card(
        card_header("Training Controls"),
        sliderInput("split_churn", "Train/Test Split", min = 0.5, max = 0.9, value = 0.8),
        actionButton("btn_train_churn", "Train Model", class = "btn-success w-100")
      ),
      card(
        card_header("Performance Metrics"),
        tableOutput("churn_metrics")
      )
    ),
    layout_columns(
      card(
        card_header("ROC Curve"),
        plotlyOutput("churn_roc")
      ),
      card(
        card_header("PR Curve"),
        plotlyOutput("churn_pr")
      )
    ),
    card(
      card_header("High Risk Accounts"),
      DTOutput("churn_risks")
    )
  ),
  
  nav_panel("Upsell Model",
    layout_columns(
        card(
          card_header("Training Controls"),
          sliderInput("split_upsell", "Train/Test Split", min = 0.5, max = 0.9, value = 0.8),
          actionButton("btn_train_upsell", "Train Model", class = "btn-success w-100")
        ),
        card(
          card_header("Lift Chart"),
          plotlyOutput("upsell_lift")
        )
    ),
    card(
      card_header("Upsell Opportunities"),
      DTOutput("upsell_preds")
    )
  )
)
