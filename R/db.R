#' Database Helper Functions
#'
#' @description
#' Helper functions to interact with the SQLite database.

library(DBI)
library(RSQLite)

#' Get Database Connection
#'
#' @param db_path Path to the SQLite database file.
#' @return A DBI connection object.
get_db_conn <- function(db_path = "predictR.sqlite") {
  dbConnect(RSQLite::SQLite(), db_path)
}

#' Initialize Database
#'
#' @param db_path Path to the SQLite database file.
#' @param schema_path Path to the SQL schema file.
#' @return NULL
init_db <- function(db_path = "predictR.sqlite", schema_path = "sql/schema.sql") {
  conn <- get_db_conn(db_path)
  on.exit(dbDisconnect(conn))
  
  schema_sql <- readLines(schema_path, warn = FALSE)
  # Split by semicolon to execute multiple statements
  statements <- unlist(strsplit(paste(schema_sql, collapse = "\n"), ";"))
  
  for (stmt in statements) {
    if (trimws(stmt) != "") {
      dbExecute(conn, stmt)
    }
  }
  message("Database initialized at ", db_path)
}

#' Write Table to Database
#'
#' @param conn DBI connection object.
#' @param table_name Name of the table to write to.
#' @param data Data frame to write.
#' @param overwrite Logical, whether to overwrite existing table.
#' @return NULL
write_table <- function(conn, table_name, data, overwrite = TRUE) {
  dbWriteTable(conn, table_name, data, overwrite = overwrite, append = !overwrite)
}

#' Read Table from Database
#'
#' @param conn DBI connection object.
#' @param table_name Name of the table to read.
#' @return Data frame.
read_table <- function(conn, table_name) {
  dbReadTable(conn, table_name)
}
