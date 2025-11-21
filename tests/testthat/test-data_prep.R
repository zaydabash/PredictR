test_that("validate_schema checks required columns", {
  df <- data.frame(a = 1, b = 2)
  expect_true(validate_schema(df, c("a", "b")))
  expect_error(validate_schema(df, c("a", "c")), "Missing columns: c")
})

test_that("load_and_clean_data returns correct structure", {
  # Mock data paths (using sample data)
  events_path <- "../../data/sample/sample_events.csv"
  accounts_path <- "../../data/sample/sample_accounts.csv"
  subs_path <- "../../data/sample/sample_subscriptions.csv"
  
  # Skip if files don't exist (e.g. in some CI envs without data)
  skip_if_not(file.exists(events_path))
  
  res <- load_and_clean_data(events_path, accounts_path, subs_path)
  
  expect_type(res, "list")
  expect_named(res, c("events", "accounts", "subscriptions"))
  expect_s3_class(res$accounts$signup_date, "Date")
})
