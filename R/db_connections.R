library(DBI)
library(RPostgres)

# This securely loads your local passwords when you are coding in RStudio, 
# but safely ignores it when running in Posit Connect Cloud!
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

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