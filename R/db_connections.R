# R/db_connections.R
library(DBI)
library(RPostgres)

# Function to securely connect to Supabase
connect_supabase <- function() {
  # Explicitly load the environment variables
  readRenviron(".Renviron")
  
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT")),
    dbname   = Sys.getenv("SUPABASE_DB"),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASS")
  )
}