# =========================================================================
# AUTOMATED EMAIL REMINDER ENGINE
# Run via Windows Task Scheduler daily to process Action Reminders
# =========================================================================

# 1. Load required libraries
library(DBI)
library(RPostgres)
library(blastula)
library(dplyr)
library(lubridate)

# Set your working directory to the app's root folder if running via Task Scheduler
setwd("C:/Users/adamsc/Documents/R/SWAP/SWAPActionTracking_DEV/SWAP2025-ActionTracking-DEV")

# Replace with the exact path to your app's folder!
readRenviron("C:/Users/adamsc/Documents/R/SWAP/SWAPActionTracking_DEV/SWAP2025-ActionTracking-DEV/.Renviron")

# App URL to include in the email
app_url <- "https://cpw-sct-swap2025-actiontracking-dev.share.connect.posit.cloud/"

# 2. Connect to the Database
db <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("SUPABASE_DB"),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")),
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASSWORD")
)

message(paste("Starting Email Engine at", Sys.time()))

tryCatch({
  # 3. Query all due reminders (Dates today or in the past)
  q_due <- "
    SELECT 
      r.reminder_id, r.implementedactionid, r.userid, r.reminder_date, r.frequency,
      p.email, p.first_name,
      l2.actionl2code || '. ' || l2.actionl2name AS action_name
    FROM track.action_reminders r
    JOIN public.profiles p ON r.userid = p.id  -- THE FIX: Removed ::text here
    JOIN track.implementedactions a ON r.implementedactionid = a.implementedactionid
    LEFT JOIN proj.l2_actions l2 ON a.actionl2id = l2.actionl2id
    WHERE r.reminder_date <= CURRENT_DATE
    "
  due_reminders <- dbGetQuery(db, q_due)
  
  if (nrow(due_reminders) == 0) {
    message("No reminders due today. Shutting down cleanly.")
  } else {
    message(paste("Found", nrow(due_reminders), "reminders to process!"))
    
    Sys.setenv(ACTIVE_GMAIL_USER = Sys.getenv("GMAIL_USER")) 
    Sys.setenv(ACTIVE_GMAIL_PASS = Sys.getenv("GMAIL_APP_PASSWORD"))
    
    # Blastula grabs the password from the active memory
    creds <- creds_envvar(
      user = Sys.getenv("MY_GMAIL_USER"),
      pass_envvar = "MY_GMAIL_PASS", 
      provider = "gmail"
    )
    
    # 5. Process each reminder
    for (i in 1:nrow(due_reminders)) {
      row <- due_reminders[i, ]
      
      message(paste("Sending email to", row$email, "for Action ID:", row$implementedactionid))
      
      # Build the email body using Blastula
      email_body <- compose_email(
        body = md(glue::glue(
          "
          {row$first_name},
          
          This is an automated reminder from the **CPW SWAP Action Tracker**.
          
          It is time to log a progress update for your conservation action:
          ### {row$action_name} (ID: {row$implementedactionid})
          
          Keeping action logs up to date ensures accurate statewide reporting. Please click the link below to  sign in, access your dashboard, navigate to **Update Action**, and submit a status update.
          
          [Log Into the Action Tracker]({app_url})
          
          *This reminder was scheduled for {row$reminder_date} with a frequency of: {row$frequency}.*
          "
        )),
        footer = md("Colorado Parks and Wildlife - Species Conservation Team")
      )
      
      # Send the email
      smtp_send(
        email = email_body,
        to = row$email,
        from = Sys.getenv("GMAIL_USER"),
        subject = paste("SWAP Action Update Reminder:", row$action_name),
        credentials = creds
      )
      
      # 6. Database Cleanup (Delete or Reschedule)
      if (row$frequency == "One-time") {
        dbExecute(db, "DELETE FROM track.action_reminders WHERE reminder_id = $1", params = list(row$reminder_id))
        message(" -> Deleted One-time reminder.")
      } else {
        # Reschedule using Postgres interval math
        interval_sql <- switch(row$frequency,
                               "Weekly" = "INTERVAL '1 week'",
                               "Monthly" = "INTERVAL '1 month'",
                               "Annually" = "INTERVAL '1 year'")
        
        q_reschedule <- paste0("UPDATE track.action_reminders SET reminder_date = CURRENT_DATE + ", interval_sql, " WHERE reminder_id = $1")
        dbExecute(db, q_reschedule, params = list(row$reminder_id))
        message(paste(" -> Rescheduled", row$frequency, "reminder."))
      }
    }
    message("All emails sent successfully!")
  }
  
}, error = function(e) {
  message(paste("FATAL ERROR in Email Engine:", e$message))
}, finally = {
  # Always close the database connection when done
  dbDisconnect(db)
  message("Database connection closed. Engine shutdown.")
})