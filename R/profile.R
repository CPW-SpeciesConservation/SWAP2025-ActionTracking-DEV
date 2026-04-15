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
      
      # T3: Admin Tools (UI handles the hiding logic via server output)
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
    
    output$admin_users_ui <- renderUI({
      req(current_user()$role == "admin")
      admin_ui_users(ns("admin_logic")) 
    })
    
    output$admin_delegation_ui <- renderUI({
      req(current_user()$role == "admin")
      admin_ui_actions(ns("admin_logic"))
    })
    
    # Start the Admin Server logic if user is admin
    observe({
      req(current_user())
      if(current_user()$role == "admin") {
        admin_server("admin_logic", db, current_user, db_sync_trigger)
      }
    })
    
    # Personal info
    
    # Dynamic Admin Badge 
    output$admin_badge_ui <- renderUI({
      req(current_user())
      if (current_user()$role == "admin") {
        div(class = "mb-3", 
            span(class = "badge bg-warning text-dark px-2 py-1", style = "font-size: 0.9em;", "Admin")
        )
      } else {
        NULL 
      }
    })
    
    observe({
      req(current_user())
      agencies <- dbGetQuery(db, "SELECT agencyname FROM lkup.Agency ORDER BY agencyname")
      updateSelectInput(session, "agency", choices = agencies$agency_name)
      prof <- dbGetQuery(db, "SELECT * FROM public.profiles WHERE id = $1", params = list(current_user()$user_id))
      if(nrow(prof) > 0) {
        updateTextInput(session, "first_name", value = prof$first_name)
        updateTextInput(session, "last_name", value = prof$last_name)
        updateTextInput(session, "job_title", value = prof$job_title)
        updateTextInput(session, "phone", value = prof$phone)
        updateSelectInput(session, "agency", 
                          choices = agencies$agencyname, 
                          selected = prof$agency)
      }
    })
    
    output$display_email <- renderText({ 
      req(current_user()) 
      current_user()$email 
    })
    
    # Save Profile
    observeEvent(input$save_profile, {
      dbExecute(db, "UPDATE public.profiles SET first_name=$1, last_name=$2, job_title=$3, phone=$4, agency=$5 WHERE id=$6",
                params = list(input$first_name, input$last_name, input$job_title, input$phone, input$agency, current_user()$user_id))
      showNotification("Profile updated!", type = "message")
    })
    
    # Update Password via Supabase Auth API
    observeEvent(input$change_pwd, {
      req(input$new_pwd) 
      req(current_user())
      
      token <- current_user()$token 
      
      if (is.null(token)) {
        showNotification("Error: Session token missing. Please log out and log back in to change your password.", type = "error")
        return()
      }
      
      supabase_url <- paste0(Sys.getenv("SUPABASE_URL"), "/auth/v1/user")
      anon_key <- Sys.getenv("SUPABASE_ANON_KEY")
      
      res <- httr::PUT(
        url = supabase_url,
        httr::add_headers(
          apikey = anon_key,
          Authorization = paste("Bearer", token)
        ),
        body = list(
          password = input$new_pwd,
          current_password = input$current_pwd
        ),
        encode = "json"
      )
      
      if (httr::status_code(res) == 200) {
        showNotification("Password successfully updated!", type = "message")
        updateTextInput(session, "current_pwd", value = "") 
        updateTextInput(session, "new_pwd", value = "")
      } else {
        error_content <- httr::content(res)
        error_msg <- if(!is.null(error_content$msg)) error_content$msg else "Invalid request."
        showNotification(paste("Failed to update password:", error_msg), type = "error")
      }
    })
    
    # My Actions
    
    my_actions_data <- reactive({
      req(current_user())
      db_sync_trigger()
      query <- "
        SELECT DISTINCT ia.implementedactionid AS \"ID\", l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", ia.status AS \"Status\"
        FROM track.implementedactions ia
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1::text OR du.userid = $1::text
      "
      dbGetQuery(db, query, params = list(current_user()$user_id))
    })
    
    # Render the scrolling table
    output$user_actions_table <- renderDT({
      datatable(
        my_actions_data(), 
        selection = 'single', # Restrict to single row selection
        rownames = FALSE, 
        options = list(
          paging = FALSE,          
          scrollY = "500px",
          dom="ft"
        )
      )
    })
    
    output$action_details_ui <- renderUI({
      req(input$user_actions_table_rows_selected)
      
      # Grab the actual ID from the selected row
      selected_idx <- input$user_actions_table_rows_selected
      action_id <- my_actions_data()$ID[selected_idx]
      
      q_core <- "SELECT l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", ia.timeframe, ia.status, ia.actiondesc AS actiondescription 
                 FROM track.implementedactions ia 
                 LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id 
                 WHERE ia.implementedactionid = $1"
      core_info <- dbGetQuery(db, q_core, params = list(action_id))
      
      q_targ <- "SELECT 
                   COALESCE(s.commonname, h.habitatsubtypename, 'Unknown Target') AS targetname, 
                   CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS targettype,
                   NULL AS actiondetail
                 FROM track.specieshabitatactions sha 
                 LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
                 LEFT JOIN proj.habitatsubtypes h ON sha.habitatsubtypeid = h.habitatsubtypeid
                 WHERE sha.implementedactionid = $1"
      targ_info <- dbGetQuery(db, q_targ, params = list(action_id))
      
      q_thr <- "SELECT 
                  l2.threatl2code || '. ' || l2.threatl2name AS threat_name, 
                  ta.justification,
                  CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_label
                FROM track.threatsaddressed ta 
                JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid 
                JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id 
                LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
                LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
                WHERE sha.implementedactionid = $1
                ORDER BY target_label ASC, threat_name ASC"
      
      thr_info <- dbGetQuery(db, q_thr, params = list(as.integer(action_id)))
      
      q_log <- "SELECT p.actiondate AS logdate, p.stattype AS metric, p.stat AS value, p.comments AS notes, u.first_name, u.last_name 
                FROM track.actiontracking p 
                LEFT JOIN public.profiles u ON p.createdby = u.id::text 
                WHERE p.implementedactionid = $1 
                ORDER BY p.actiondate DESC LIMIT 1"
      
      log_info <- dbGetQuery(db, q_log, params = list(action_id))
      
      targ_html <- if(nrow(targ_info) > 0) {
        HTML(paste0("<ul style='padding-left: 15px;'>", paste0("<li><b>", targ_info$targetname, "</b> (", targ_info$targettype, ")<br><span style='color:grey;'><i>Detail: ", ifelse(is.na(targ_info$actiondetail) | targ_info$actiondetail == "", "None", targ_info$actiondetail), "</i></span></li>", collapse = ""), "</ul>"))
      } else { HTML("<i>No targets assigned.</i>") }
      
      thr_html <- if(nrow(thr_info) > 0) {
        HTML(paste0(
          "<ul style='padding-left: 15px;'>", 
          paste0("<li><b>", thr_info$threat_name, "</b> <span style='color: #0D67B8; font-size: 0.9em; font-weight: bold;'>[", thr_info$target_label, "]</span><br><span style='color:grey;'><i>Justification: ", ifelse(is.na(thr_info$justification) | trimws(thr_info$justification) == "", "None", thr_info$justification), "</i></span></li><br>", collapse = ""), 
          "</ul>"
        ))
      } else { HTML("<i>No threats mitigated.</i>") }
      
      log_html <- if(nrow(log_info) > 0) {
        HTML(paste0(
          "<div style='line-height: 1.5; font-size: 0.95em;'>",
          "<b>Date:</b> ", as.character(log_info$logdate), "<br>",
          "<b>Logged By:</b> ", log_info$first_name, " ", log_info$last_name, "<br>",
          "<b>Metric (", log_info$metric, "):</b> ", log_info$value, "<br>",
          "<b>Notes:</b> <i>", ifelse(is.na(log_info$notes) | trimws(log_info$notes) == "", "No notes provided.", log_info$notes), "</i>",
          "</div>"
        ))
      } else { HTML("<i>No updates have been logged yet.</i>") }
      
      # -- Build the Output UI --
      fluidRow(
        # Top Row: Core Info (Spans the full 12 columns of this right-hand area)
        column(12, 
               card(
                 card_header("Details", style = "background-color: #07234c; color: white; font-weight: bold;"),
                 card_body(
                   HTML(paste0(
                     "<div style='line-height: 1.6; font-size: 0.95em;'>",
                     "<b>Action:</b> ", core_info$Action, "<br>",
                     "<b>Timeframe:</b> ", core_info$timeframe, "<br>",
                     "<b>Status:</b> ", core_info$status, "<br>",
                     "<b>User Description:</b> <i>", 
                     ifelse(is.na(core_info$actiondescription) | trimws(core_info$actiondescription) == "", "No custom description provided.", core_info$actiondescription),
                     "</i></div>"
                   ))
                 )
               )
        ),
        
        # Bottom Row: 3 Detail Cards (Spanning 4 columns each)
        column(4,
               card(
                 card_header("Targeted Species/Habitats", style = "background-color: #0D67B8; color: white; font-weight: bold; font-size: 0.9em;"),
                 card_body(style = "padding: 10px;", targ_html)
               )
        ),
        column(4,
               card(
                 card_header("Mitigated Threats", style = "background-color: #A55A3D; color: white; font-weight: bold; font-size: 0.9em;"),
                 card_body(style = "padding: 10px;", thr_html)
               )
        ),
        column(4,
               card(
                 card_header("Most Recent Update", style = "background-color: #E1A11E; color: white; font-weight: bold; font-size: 0.9em;"),
                 card_body(style = "padding: 10px;", log_html)
               )
        )
      )
    })

    return(
      list(
        go_login = reactive({ input$logout_btn })
      )
    )
  })
}