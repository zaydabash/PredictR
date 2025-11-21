# UI Definition

ui <- navbarPage(
  title = "PredictR",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Tab 1: Data Management
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        h4("Upload Data"),
        fileInput("file_events", "Events CSV", accept = ".csv"),
        fileInput("file_accounts", "Accounts CSV", accept = ".csv"),
        fileInput("file_subs", "Subscriptions CSV", accept = ".csv"),
        actionButton("btn_ingest", "Ingest Data", class = "btn-primary"),
        hr(),
        actionButton("btn_load_sample", "Load Sample Data", class = "btn-info"),
        hr(),
        textOutput("status_msg")
      ),
      mainPanel(
        h4("Data Summary"),
        tableOutput("table_summary")
      )
    )
  ),
  
  # Tab 2: Churn Model
  tabPanel("Churn Model",
    sidebarLayout(
      sidebarPanel(
        h4("Train Churn Model"),
        sliderInput("split_churn", "Train/Test Split", min = 0.5, max = 0.9, value = 0.8),
        actionButton("btn_train_churn", "Train Model", class = "btn-success")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Metrics", tableOutput("churn_metrics")),
          tabPanel("ROC Curve", plotlyOutput("churn_roc")),
          tabPanel("High Risk Accounts", DTOutput("churn_risks"))
        )
      )
    )
  ),
  
  # Tab 3: Upsell Model
  tabPanel("Upsell Model",
    sidebarLayout(
      sidebarPanel(
        h4("Train Upsell Model"),
        sliderInput("split_upsell", "Train/Test Split", min = 0.5, max = 0.9, value = 0.8),
        actionButton("btn_train_upsell", "Train Model", class = "btn-success")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Metrics", tableOutput("upsell_metrics")),
          tabPanel("Predictions", DTOutput("upsell_preds"))
        )
      )
    )
  ),
  
  # Tab 4: Revenue Overview
  tabPanel("Revenue Overview",
    mainPanel(
      h4("Monthly Recurring Revenue (MRR) Distribution"),
      plotlyOutput("plot_mrr_dist"),
      h4("Plan Distribution"),
      plotlyOutput("plot_plan_dist")
    )
  )
)
