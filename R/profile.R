profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("My Profile", class = "mb-4", style = "color: #07234C; font-weight: bold;"),
    
    # Nested navigation within the Profile page
    navset_underline(
      id = ns("profile_tabs"),
      
      # T1: Personal Info (Everyone sees this)
      nav_panel("Personal Information",
                div(class = "mt-4",
                    layout_columns(
                      div(class = "card shadow-sm",
                          div(class = "card-header bg-primary text-white", "Personal Information"),
                          div(class = "card-body",
                              uiOutput(ns("admin_badge_ui")),
                              textInput(ns("first_name"), "First Name"),
                              textInput(ns("last_name"), "Last Name"),
                              textInput(ns("job_title"), "Job Title"),
                              textInput(ns("phone"), "Phone Number"),
                              selectInput(ns("agency"), "Agency", choices = c("Loading..." = "")),
                              actionButton(ns("save_profile"), "Save Changes", class = "btn-primary w-100 mt-3")
                          )
                      ),
                      div(class = "card shadow-sm",
                          div(class = "card-header bg-dark text-white", "Security Settings"),
                          div(class = "card-body",
                              p(strong("Registered Email: "), textOutput(ns("display_email"), inline = TRUE)),
                              hr(),
                              passwordInput(ns("current_pwd"), "Current Password"),
                              passwordInput(ns("new_pwd"), "New Password"),
                              actionButton(ns("change_pwd"), "Update Password", class = "btn-secondary w-100 mt-2"),
                              
                              hr(),
                              actionButton(ns("logout_btn"), "Log Out", class = "btn-outline-danger w-100 mt-4")
                          )
                      )
                    )
                )
      ),
      
      # T2: My Actions (Everyone sees this)
      nav_panel("My Actions",
                fluidRow(class = "mt-4",
                         column(4, 
                                div(class = "card shadow-sm",
                                    div(class = "card-header bg-success text-white", "User Created or Delegated Actions"),
                                    div(class = "card-body",
                                        DTOutput(ns("user_actions_table"))
                                    )
                                )
                         ),
                         column(8,
                                uiOutput(ns("action_details_ui"))
                         )
                )
      ),
      
      # T3: Admin Tools 
      nav_panel("User Management", value = "admin_users_tab",
                uiOutput(ns("admin_users_ui"))
      ),
      
      nav_panel("Action Delegation", value = "admin_actions_tab",
                uiOutput(ns("admin_delegation_ui"))
      )
    )
  )
}

profile_server <- function(id, db, current_user, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    
    observe({
      req(current_user())
      if (current_user()$role != "admin") {
        nav_hide(id = "profile_tabs", target = "admin_users_tab")
        nav_hide(id = "profile_tabs", target = "admin_actions_tab")
      } else {
        nav_show(id = "profile_tabs", target = "admin_users_tab")
        nav_show(id = "profile_tabs", target = "admin_actions_tab")
      }
    })
    
    output$admin_users_ui <- renderUI({ req(current_user()$role == "admin"); admin_ui_users(ns("admin_logic")) })
    output$admin_delegation_ui <- renderUI({ req(current_user()$role == "admin"); admin_ui_actions(ns("admin_logic")) })
    
    
    admin_server("admin_logic", db, current_user, db_sync_trigger)
    
    output$admin_badge_ui <- renderUI({ req(current_user()); if (current_user()$role == "admin") { div(class = "mb-3", span(class = "badge bg-warning text-dark px-2 py-1", style = "font-size: 0.9em;", "Admin")) } else { NULL } })
    
    observe({
      req(current_user())
      agencies <- dbGetQuery(db, "SELECT agencyname FROM lkup.Agency ORDER BY agencyname")
      updateSelectInput(session, "agency", choices = agencies$agencyname)
      prof <- dbGetQuery(db, "SELECT * FROM public.profiles WHERE id = $1", params = list(current_user()$user_id))
      if(nrow(prof) > 0) {
        updateTextInput(session, "first_name", value = prof$first_name)
        updateTextInput(session, "last_name", value = prof$last_name)
        updateTextInput(session, "job_title", value = prof$job_title)
        updateTextInput(session, "phone", value = prof$phone)
        updateSelectInput(session, "agency", choices = agencies$agencyname, selected = prof$agency)
      }
    })
    
    output$display_email <- renderText({ req(current_user()); current_user()$email })
    
    observeEvent(input$save_profile, {
      dbExecute(db, "UPDATE public.profiles SET first_name=$1, last_name=$2, job_title=$3, phone=$4, agency=$5 WHERE id=$6",
                params = list(input$first_name, input$last_name, input$job_title, input$phone, input$agency, current_user()$user_id))
      showNotification("Profile updated!", type = "message")
    })
    
    observeEvent(input$change_pwd, {
      req(input$new_pwd); req(current_user()); token <- current_user()$token 
      if (is.null(token)) { showNotification("Error: Session token missing. Please log out and log back in to change your password.", type = "error"); return() }
      res <- httr::PUT(url = paste0(Sys.getenv("SUPABASE_URL"), "/auth/v1/user"), httr::add_headers(apikey = Sys.getenv("SUPABASE_ANON_KEY"), Authorization = paste("Bearer", token)), body = list(password = input$new_pwd, current_password = input$current_pwd), encode = "json")
      if (httr::status_code(res) == 200) { showNotification("Password successfully updated!", type = "message"); updateTextInput(session, "current_pwd", value = ""); updateTextInput(session, "new_pwd", value = "")
      } else { error_content <- httr::content(res); error_msg <- if(!is.null(error_content$msg)) error_content$msg else "Invalid request."; showNotification(paste("Failed to update password:", error_msg), type = "error") }
    })
    
    my_actions_data <- reactive({
      req(current_user()); db_sync_trigger()
      query <- "SELECT DISTINCT ia.implementedactionid AS \"ID\", l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", ia.implementation_progress AS \"Status\" FROM track.implementedactions ia LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid WHERE ia.createdby = $1::text OR du.userid = $1::text ORDER BY \"ID\" DESC"
      df <- dbGetQuery(db, query, params = list(current_user()$user_id))
      if(nrow(df) > 0) df$Status <- sapply(df$Status, function(s) as.character(get_status_badge(s)))
      df
    })
    
    output$user_actions_table <- renderDT({ datatable(my_actions_data(), escape = FALSE, selection = 'single', rownames = FALSE, options = list(paging = FALSE, scrollY = "500px", dom="ft")) })
    
    # -----------------------------------------------------
    # REMINDER CRUD LOGIC
    # -----------------------------------------------------
    observeEvent(input$btn_add_reminder, {
      req(input$user_actions_table_rows_selected, input$new_rem_date, input$new_rem_freq)
      action_id <- my_actions_data()$ID[input$user_actions_table_rows_selected]
      
      dbExecute(db, "INSERT INTO track.action_reminders (implementedactionid, userid, reminder_date, frequency) VALUES ($1, $2, $3, $4)",
                params = list(as.integer(action_id), current_user()$user_id, as.character(input$new_rem_date), input$new_rem_freq))
      showNotification("Email reminder scheduled successfully!", type="message")
      db_sync_trigger(db_sync_trigger() + 1) # Force UI refresh
    })
    
    # Step 1: User clicks trash → store the ID and show confirmation modal
    observeEvent(input$pending_del_reminder_id, {
      req(input$pending_del_reminder_id)
      showModal(modalDialog(
        title = "Delete Reminder?",
        p("Are you sure you want to delete this reminder? This cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_del_reminder"), "Yes, Delete It", class = "btn-danger", style = "font-weight: bold;")
        ),
        easyClose = TRUE
      ))
    })
    
    # Step 2: User confirms → delete and dismiss
    observeEvent(input$confirm_del_reminder, {
      req(input$pending_del_reminder_id)
      dbExecute(db, "DELETE FROM track.action_reminders WHERE reminder_id = $1", params = list(as.integer(input$pending_del_reminder_id)))
      removeModal()
      showNotification("Reminder deleted.", type = "message")
      db_sync_trigger(db_sync_trigger() + 1)
    })
    
    output$action_details_ui <- renderUI({
      req(input$user_actions_table_rows_selected)
      db_sync_trigger() # Ensure this triggers when a reminder is added/deleted
      
      action_id <- my_actions_data()$ID[input$user_actions_table_rows_selected]
      
      q_core <- "SELECT l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", ia.timeframe, ia.implementation_progress, ia.result_progress, ia.actiondesc AS actiondescription FROM track.implementedactions ia LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id WHERE ia.implementedactionid = $1"
      core_info <- dbGetQuery(db, q_core, params = list(action_id))
      
      q_targ <- "SELECT COALESCE(s.commonname, h.habitatsubtypename, 'Unknown Target') AS targetname, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS targettype, NULL AS actiondetail FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes h ON sha.habitatsubtypeid = h.habitatsubtypeid WHERE sha.implementedactionid = $1"
      targ_info <- dbGetQuery(db, q_targ, params = list(action_id))
      
      q_thr <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS threat_name, ta.justification, ta.threatl2id, ta.alternative_category, ta.justification_text, CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_label FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 ORDER BY target_label ASC, threat_name ASC"
      thr_info <- dbGetQuery(db, q_thr, params = list(as.integer(action_id)))
      
      q_log <- "SELECT TO_CHAR(p.actiondate, 'MM/DD/YYYY') AS logdate, p.implementation_progress, p.result_progress, p.what_done, u.first_name, u.last_name FROM track.actiontracking p LEFT JOIN public.profiles u ON p.createdby = u.id::text WHERE p.implementedactionid = $1 ORDER BY p.actiondate DESC LIMIT 1"
      log_info <- dbGetQuery(db, q_log, params = list(action_id))
      
      # Fetch Reminders
      q_rem <- "SELECT reminder_id, TO_CHAR(reminder_date, 'MM/DD/YYYY') as formatted_date, frequency FROM track.action_reminders WHERE implementedactionid = $1 AND userid = $2 ORDER BY reminder_date ASC"
      reminders <- dbGetQuery(db, q_rem, params = list(as.integer(action_id), current_user()$user_id))
      
      targ_html <- if(nrow(targ_info) > 0) { HTML(paste0("<ul style='padding-left: 15px;'>", paste0("<li><b>", targ_info$targetname, "</b> (", targ_info$targettype, ")<br><span style='color:grey;'><i>Detail: ", ifelse(is.na(targ_info$actiondetail) | targ_info$actiondetail == "", "None", targ_info$actiondetail), "</i></span></li>", collapse = ""), "</ul>")) } else { HTML("<i>No targets assigned.</i>") }
      
      thr_html <- if(nrow(thr_info) > 0) {
        thr_info$group_name <- ifelse(thr_info$threatl2id == 59, paste0("Broader Goal: ", thr_info$alternative_category), thr_info$threat_name)
        HTML(paste0("<ul style='padding-left: 15px;'>", paste0(lapply(unique(thr_info$group_name), function(g) {
          sub_df <- thr_info[thr_info$group_name == g, ]; is_broader <- sub_df$threatl2id[1] == 59
          title <- if(is_broader) paste0("<span style='color: #0D67B8;'>Broader Goal: ", sub_df$alternative_category[1], "</span>") else sub_df$threat_name[1]
          t_list <- paste0("<ul style='list-style-type:circle; margin-top: 5px; margin-bottom: 15px;'>", paste0(lapply(1:nrow(sub_df), function(i) { just_text <- if(is_broader) sub_df$justification_text[i] else sub_df$justification[i]; paste0("<li><span style='color: #0D67B8; font-weight: bold;'>[", sub_df$target_label[i], "]</span><br><span style='color:grey;'><i>Justification: ", ifelse(is.na(just_text) || trimws(just_text)=="", "None", just_text), "</i></span></li>") }), collapse = ""), "</ul>")
          paste0("<li><b>", title, "</b>", t_list, "</li>")
        }), collapse = ""), "</ul>"))
      } else { HTML("<i>No threats mitigated.</i>") }
      
      log_html <- if(nrow(log_info) > 0) {
        HTML(paste0("<div style='line-height: 1.5; font-size: 0.95em;'>", "<b>Date:</b> ", as.character(log_info$logdate), "<br>", "<b>Logged By:</b> ", log_info$first_name, " ", log_info$last_name, "<br>", "<b>Implementation:</b> ", get_status_badge(log_info$implementation_progress), "<br>", "<b>Effectiveness:</b> ", get_status_badge(log_info$result_progress), "<br>", "<b>Notes:</b> <i>", ifelse(is.na(log_info$what_done) | trimws(log_info$what_done) == "", "No notes provided.", log_info$what_done), "</i>", "</div>"))
      } else { HTML("<i>No updates have been logged yet.</i>") }
      
      # Dynamic Reminders List HTML
      rem_list_html <- if(nrow(reminders) > 0) {
        tags$ul(class="list-group mb-3 shadow-sm", lapply(1:nrow(reminders), function(i) {
          tags$li(class="list-group-item d-flex justify-content-between align-items-center p-2",
                  tagList(span(class="fw-bold", style="color: #07234c;", reminders$formatted_date[i]), span(class="text-muted ms-2 small", paste0("(", reminders$frequency[i], ")"))),
                  tags$button(class="btn btn-sm btn-outline-danger border-0", icon("trash"),
                              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("pending_del_reminder_id"), reminders$reminder_id[i])))
        }))
      } else {
        p(em("No active reminders for this action.", class="text-muted text-center d-block mb-3"))
      }
      
      fluidRow(
        # Top Row: 6-Col Details / 6-Col Reminders
        column(6, 
               div(class = "card shadow-sm mb-3",
                   div(class = "card-header", style = "background-color: #07234c; color: white; font-weight: bold;", "Action Details"),
                   div(class = "card-body", style="min-height: 250px;",
                       HTML(paste0("<div style='line-height: 1.6; font-size: 0.95em;'>", "<b>Action:</b> ", core_info$Action, "<br>", "<b>Timeframe:</b> ", core_info$timeframe, "<br>", "<b>Implementation:</b> ", get_status_badge(core_info$implementation_progress), "<br>", "<b>Effectiveness:</b> ", get_status_badge(core_info$result_progress), "<br>", "<b>User Description:</b> <i>", ifelse(is.na(core_info$actiondescription) | trimws(core_info$actiondescription) == "", "No custom description provided.", core_info$actiondescription), "</i></div>"))
                   )
               )
        ),
        
        column(6,
               div(class = "card shadow-sm mb-3",
                   div(class = "card-header", style = "background-color: #055A53; color: white; font-weight: bold;", "Email Reminders"),
                   div(class = "card-body", style="min-height: 250px;",
                       rem_list_html,
                       div(class="pt-3 border-top",
                           layout_columns(
                             dateInput(ns("new_rem_date"), "Reminder Date", value = Sys.Date() + 365, width="100%"),
                             selectInput(ns("new_rem_freq"), "Frequency", choices = c("One-time", "Weekly", "Monthly", "Annually"), width="100%")
                           ),
                           actionButton(ns("btn_add_reminder"), "Set Reminder", class="btn-success w-100 fw-bold")
                       )
                   )
               )
        ),
        
        # Bottom Row: 3 Detail Cards (Spanning 4 columns each)
        column(4, div(class = "card shadow-sm", div(class = "card-header", style = "background-color: #0D67B8; color: white; font-weight: bold; font-size: 0.9em;", "Targeted Species/Habitats"), div(class = "card-body", style = "padding: 10px;", targ_html))),
        column(4, div(class = "card shadow-sm", div(class = "card-header", style = "background-color: #A55A3D; color: white; font-weight: bold; font-size: 0.9em;", "Mitigated Threats"), div(class = "card-body", style = "padding: 10px;", thr_html))),
        column(4, div(class = "card shadow-sm", div(class = "card-header", style = "background-color: #E1A11E; color: white; font-weight: bold; font-size: 0.9em;", "Most Recent Update"), div(class = "card-body", style = "padding: 10px;", log_html)))
      )
    })
    
    return(
      list(
        go_login = reactive({ input$logout_btn })
      )
    )
  })
}