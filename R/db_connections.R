library(DBI)
library(RPostgres)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

connect_supabase <- function() {
  tryCatch({
    dbConnect(
      RPostgres::Postgres(),
      dbname   = Sys.getenv("SUPABASE_DB"),
      host     = Sys.getenv("SUPABASE_HOST"),
      port     = as.integer(Sys.getenv("SUPABASE_PORT")),
      user     = Sys.getenv("SUPABASE_USER"),
      password = Sys.getenv("SUPABASE_PASSWORD")
    )
  }, error = function(e) {
    # If it fails, print this giant red error to the Posit logs but don't crash!
    message("===================================================")
    message("DATABASE CRASH CAUGHT! HERE IS THE EXACT ERROR:")
    message(e$message)
    message("===================================================")
    return(NULL)
  })
}