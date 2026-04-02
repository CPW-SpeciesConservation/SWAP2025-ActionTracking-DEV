library(DBI)
library(RPostgres)

# Safely load local variables, but ignore this step in the cloud!
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Wrap the connection in the function your app is looking for
connect_supabase <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("SUPABASE_DB"),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT")),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD")
  )
}