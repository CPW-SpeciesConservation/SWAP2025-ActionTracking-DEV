profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("User Profile & Management", class = "mb-4", style = "color: #07234C; font-weight: bold;"),
    
    # Nested navigation within the Profile page
    navset_underline(
      id = ns("profile_tabs"),
      
      # T1: Personal Info (Everyone sees this)
      nav_panel("Personal Information",
                div(class = "mt-4",
                    layout_columns(
                      div(class = "card shadow-sm",
                          div(class = "card-header bg-primary text-white", "Identity & Agency"),
                          div(class = "card-body",
                              textInput(ns("first_name"), "First Name"),
                              textInput(ns("last_name"), "Last Name"),
                              textInput(ns("job_title"), "Job Title"),
                              textInput(ns("phone"), "Phone Number"),
                              disabled(textInput(ns("agency"), "Agency (Locked)")),
                              disabled(textInput(ns("role"), "Role (Locked)")),
                              actionButton(ns("save_profile"), "Save Changes", class = "btn-primary w-100 mt-3")
                          )
                      ),
                      div(class = "card shadow-sm",
                          div(class = "card-header bg-dark text-white", "Security Settings"),
                          div(class = "card-body",
                              p(strong("Registered Email: "), textOutput(ns("display_email"), inline = TRUE)),
                              hr(),
                              
                              passwordInput(ns("new_pwd"), "New Password"),
                              actionButton(ns("change_pwd"), "Update Password", class = "btn-secondary w-100 mt-2"),
                              
                              hr(),
                              actionButton(ns("logout_btn"), "Log Out of System", class = "btn-outline-danger w-100 mt-4")
                          )
                      )
                    )
                )
      ),
      
      # T2: My Actions (Everyone sees this)
      nav_panel("My Actions",
                div(class = "card shadow-sm mt-4",
                    div(class = "card-header bg-success text-white", "Actions you created or are delegated to manage"),
                    div(class = "card-body",
                        DTOutput(ns("user_actions_table"))
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
    ns <- session$ns # This fixes the "could not find function ns" error!
    
    # --- 1. ADMIN TAB VISIBILITY ---
    # We hide the tabs in the UI by rendering nothing if the user isn't an admin
    output$admin_users_ui <- renderUI({
      req(current_user()$role == "admin")
      admin_ui_users(ns("admin_logic")) # Calling sub-functions from admin.R
    })
    
    output$admin_delegation_ui <- renderUI({
      req(current_user()$role == "admin")
      admin_ui_actions(ns("admin_logic"))
    })
    
    # Start the Admin Server logic if user is admin
    observe({
      # Again, parentheses are key!
      req(current_user())
      if(current_user()$role == "admin") {
        admin_server("admin_logic", db, current_user, db_sync_trigger)
      }
    })
    
    # --- 2. PERSONAL INFO LOGIC ---
    observe({
      req(current_user())
      prof <- dbGetQuery(db, "SELECT * FROM public.profiles WHERE id = $1", params = list(current_user()$user_id))
      if(nrow(prof) > 0) {
        updateTextInput(session, "first_name", value = prof$first_name)
        updateTextInput(session, "last_name", value = prof$last_name)
        updateTextInput(session, "job_title", value = prof$job_title)
        updateTextInput(session, "phone", value = prof$phone)
        updateTextInput(session, "agency", value = prof$agency)
        updateTextInput(session, "role", value = prof$role)
      }
    })
    
    output$display_email <- renderText({ 
      req(current_user()) 
      current_user()$email 
    })
    
    # Save Profile
    observeEvent(input$save_profile, {
      dbExecute(db, "UPDATE public.profiles SET first_name=$1, last_name=$2, job_title=$3, phone=$4 WHERE id=$5",
                params = list(input$first_name, input$last_name, input$job_title, input$phone, current_user()$user_id))
      showNotification("Profile updated!", type = "message")
    })
    
    # --- 3. MY ACTIONS LOGIC ---
    output$user_actions_table <- renderDT({
      req(current_user())
      db_sync_trigger()
      query <- "
        SELECT DISTINCT ia.implementedactionid, l2.actionl2name AS \"Action\", ia.status AS \"Status\"
        FROM track.implementedactions ia
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1::text OR du.userid = $1::text
      "
      df <- dbGetQuery(db, query, params = list(current_user()$user_id))
      datatable(df, rownames = FALSE, options = list(pageLength = 10))
    })
    

    return(
      list(
        go_login = reactive({ input$logout_btn })
      )
    )
  })
}