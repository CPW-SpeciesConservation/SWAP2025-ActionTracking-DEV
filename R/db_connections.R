library(DBI)
library(RPostgres)
library(pool) 

# Safely load local variables
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

connect_supabase <- function() {

  dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = Sys.getenv("SUPABASE_DB"),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT")),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    minSize  = 1,
    maxSize  = 10,
    idleTimeout = 3600000 # 1 hour
  )
}