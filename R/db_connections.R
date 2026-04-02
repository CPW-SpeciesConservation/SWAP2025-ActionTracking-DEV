library(DBI)
library(RPostgres)

readRenviron(".Renviron")

dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("SUPABASE_DB"),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")),
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASSWORD")
)