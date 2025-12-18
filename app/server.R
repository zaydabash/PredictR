# Server Logic

server <- function(input, output, session) {
  
  # Session ID for data isolation
  session_id <- session$token
  
  # Reactive Values
  rv <- reactiveValues(
    status = "Ready",
    features = NULL,
    churn_model = NULL,
    upsell_model = NULL
  )
  
  # Helper to get DB connection (uses centralized absolute path logic)
  get_conn <- function() {
    get_db_conn()
  }
  
  # --- Data Ingestion ---
  
  observeEvent(input$btn_ingest, {
    req(input$file_events, input$file_accounts, input$file_subs)
    
    tryCatch({
      rv$status <- "Ingesting data..."
      data <- load_and_clean_data(
        input$file_events$datapath,
        input$file_accounts$datapath,
        input$file_subs$datapath
      )
      
      conn <- get_conn()
      # Ingest with session isolation
      ingest_data(conn, data, tenant_id = session_id)
      dbDisconnect(conn)
      
      rv$status <- "Data ingestion complete."
    }, error = function(e) {
      rv$status <- paste("Error:", e$message)
    })
  })
  
  observeEvent(input$btn_load_sample, {
    tryCatch({
      rv$status <- "Loading sample data..."
      # Use absolute paths if possible, but for sample data we assume they are in project
      root <- rprojroot::find_root(rprojroot::has_dir("R"))
      data <- load_and_clean_data(
        file.path(root, "data/sample/sample_events.csv"),
        file.path(root, "data/sample/sample_accounts.csv"),
        file.path(root, "data/sample/sample_subscriptions.csv")
      )
      
      conn <- get_conn()
      ingest_data(conn, data, tenant_id = session_id)
      dbDisconnect(conn)
      
      rv$status <- "Sample data loaded."
    }, error = function(e) {
      rv$status <- paste("Error:", e$message)
    })
  })
  
  output$status_msg <- renderText({ rv$status })
  
  output$table_summary <- renderTable({
    input$btn_ingest
    input$btn_load_sample
    
    conn <- get_conn()
    on.exit(dbDisconnect(conn))
    
    if (dbExistsTable(conn, "accounts")) {
      # Filter by session_id/tenant_id
      acc <- dbGetQuery(conn, "SELECT count(*) as n FROM accounts WHERE tenant_id = ?", params = list(session_id))
      evt <- dbGetQuery(conn, "SELECT count(*) as n FROM events WHERE tenant_id = ?", params = list(session_id))
      sub <- dbGetQuery(conn, "SELECT count(*) as n FROM subscriptions WHERE tenant_id = ?", params = list(session_id))
      
      tibble(
        Table = c("Accounts", "Events", "Subscriptions"),
        Rows = c(acc$n, evt$n, sub$n)
      )
    } else {
      tibble(Message = "No data found in DB")
    }
  })
  
  # --- Feature Engineering ---
  
  observe({
    input$btn_ingest
    input$btn_load_sample
    
    conn <- get_conn()
    on.exit(dbDisconnect(conn))
    
    if (dbExistsTable(conn, "accounts")) {
      # Filter by session_id
      rv$features <- create_features(conn) %>% filter(tenant_id == session_id)
    }
  })
  
  # --- Churn Model ---
  
  observeEvent(input$btn_train_churn, {
    req(rv$features)
    withProgress(message = 'Training Churn Model...', {
      res <- train_churn_model(rv$features, split_ratio = input$split_churn)
      rv$churn_model <- res
    })
  })
  
  output$churn_metrics <- renderTable({
    req(rv$churn_model)
    preds <- predict_churn(rv$churn_model$model, rv$churn_model$test_data)
    calculate_metrics(preds, rv$churn_model$test_data$is_churned)
  })
  
  output$churn_roc <- renderPlotly({
    req(rv$churn_model)
    preds <- predict_churn(rv$churn_model$model, rv$churn_model$test_data)
    p <- plot_roc_curve(preds, rv$churn_model$test_data$is_churned)
    ggplotly(p)
  })

  output$churn_pr <- renderPlotly({
    req(rv$churn_model)
    preds <- predict_churn(rv$churn_model$model, rv$churn_model$test_data)
    p <- plot_pr_curve(preds, rv$churn_model$test_data$is_churned)
    ggplotly(p)
  })
  
  output$churn_risks <- renderDT({
    req(rv$churn_model)
    all_preds <- predict_churn(rv$churn_model$model, rv$features)
    
    rv$features %>%
      mutate(Churn_Prob = round(all_preds, 3)) %>%
      select(account_id, Churn_Prob, current_mrr, plan_tier) %>%
      arrange(desc(Churn_Prob)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # --- Upsell Model ---
  
  observeEvent(input$btn_train_upsell, {
    req(rv$features)
    withProgress(message = 'Training Upsell Model...', {
      res <- train_upsell_model(rv$features, split_ratio = input$split_upsell)
      rv$upsell_model <- res
    })
  })
  
  output$upsell_metrics <- renderTable({
    req(rv$upsell_model)
    preds <- predict_upsell(rv$upsell_model$model, rv$upsell_model$test_data)
    calculate_metrics(preds, rv$upsell_model$test_data$is_premium)
  })

  output$upsell_lift <- renderPlotly({
    req(rv$upsell_model)
    preds <- predict_upsell(rv$upsell_model$model, rv$upsell_model$test_data)
    p <- plot_lift_chart(preds, rv$upsell_model$test_data$is_premium)
    ggplotly(p)
  })
  
  output$upsell_preds <- renderDT({
    req(rv$upsell_model)
    all_preds <- predict_upsell(rv$upsell_model$model, rv$features)
    
    rv$features %>%
      mutate(Upsell_Prob = round(all_preds, 3)) %>%
      select(account_id, Upsell_Prob, current_mrr, plan_tier) %>%
      arrange(desc(Upsell_Prob)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # --- Revenue Overview ---
  
  output$plot_mrr_dist <- renderPlotly({
    req(rv$features)
    p <- ggplot(rv$features, aes(x = current_mrr)) +
      geom_histogram(fill = "#007bff", bins = 20) +
      theme_minimal() +
      labs(title = "Distribution of MRR", x = "MRR", y = "Count")
    ggplotly(p)
  })
  
  output$plot_plan_dist <- renderPlotly({
    req(rv$features)
    p <- ggplot(rv$features, aes(x = plan_tier, fill = plan_tier)) +
      geom_bar() +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Accounts by Plan Tier", x = "Plan", y = "Count")
    ggplotly(p)
  })
}
