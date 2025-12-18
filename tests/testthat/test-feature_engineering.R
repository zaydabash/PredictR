test_that("create_features generates correct columns", {
  # Setup in-memory DB for testing
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))
  
  # Create tables
  init_db_sql <- readLines("../../sql/schema.sql", warn = FALSE)
  stmts <- unlist(strsplit(paste(init_db_sql, collapse = "\n"), ";"))
  for (s in stmts) if (trimws(s) != "") DBI::dbExecute(conn, s)
  
  # Insert dummy data
  DBI::dbWriteTable(conn, "accounts", data.frame(
    account_id = 1, name = "Test", signup_date = as.character(Sys.Date() - 10), region = "NA"
  ), overwrite = TRUE)
  DBI::dbWriteTable(conn, "subscriptions", data.frame(
    subscription_id = 1, account_id = 1, plan_tier = "Pro", mrr = 100, 
    status = "Active", start_date = as.character(Sys.Date() - 10), end_date = NA
  ), overwrite = TRUE)
  DBI::dbWriteTable(conn, "events", data.frame(
    event_id = 1, account_id = 1, event_type = "login", timestamp = as.character(Sys.time())
  ), overwrite = TRUE)
  
  feats <- create_features(conn)
  
  expect_s3_class(feats, "data.frame")
  expect_true("account_age_days" %in% names(feats))
  expect_true("n_30d_login" %in% names(feats))
  expect_equal(feats$n_30d_login[1], 1)
})
