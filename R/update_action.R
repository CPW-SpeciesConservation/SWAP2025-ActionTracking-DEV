update_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Update an Existing Action", class = "mb-4"),
    p("Select an action below to view its full details and log new progress."),
    
    # 1. YOUR ACTIONS TABLE
    div(class = "card mb-4 shadow-sm",
        div(class = "card-header text-white", style = "background-color: #07234C;", "Your Actions & Delegations"),
        div(class = "card-body",
            DTOutput(ns("action_table"))
        )
    ),
    
    # This panel only shows up when a row in the table is clicked
    conditionalPanel(
      condition = sprintf("input['%s'] != null", ns("action_table_rows_selected")),
      
      # 2. FULL ACTION DETAILS
      div(class = "card mb-4 shadow-sm", style = "border-color: #07234C;",
          div(class = "card-header text-white", style = "background-color: #07234C;", "Original Action Details"),
          div(class = "card-body bg-light",
              uiOutput(ns("full_action_details"))
          )
      ),
      
      # 2.5 COLLABORATORS & DELEGATION (NEW)
      div(class = "card mb-4 shadow-sm", style = "border-color: #AA5F40;",
          div(class = "card-header text-white", style = "background-color: #AA5F40;", "Action Collaborators"),
          div(class = "card-body",
              uiOutput(ns("collaborators_ui"))
          )
      ),
      
      # 3. PREVIOUS UPDATES HISTORY 
      div(class = "card mb-4 shadow-sm",
          div(class = "card-header text-white", style = "background-color: #055A53;", "Previous Updates History"),
          div(class = "card-body",
              DTOutput(ns("history_table"))
          )
      ),
      
      # 4. ADD NEW UPDATE ENTRY 
      div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
          div(class = "card-header bg-success text-white", "Log New Progress"),
          div(class = "card-body", style = "overflow: visible;",
              
              layout_columns(
                dateInput(ns("action_date"), "Date of Update", value = Sys.Date(), width = "100%"),
                selectInput(ns("stat_type"), "Metric Type", choices = c("Percentage", "Count"), width = "100%"),
                numericInput(ns("stat_value"), "Value", value = 0, min = 0, width = "100%"),
                selectInput(ns("new_status"), "Update Action Status", 
                            choices = c("Planned", "In Progress", "Completed"), 
                            width = "100%")
              ),
              
              div(class = "mt-3",
                  textAreaInput(ns("comments"), "Update Notes", rows = 3, width = "100%")
              ),
              
              actionButton(ns("submit_update"), "Submit Update to Database", class = "btn-success btn-lg mt-3 w-100")
          )
      )
    )
  )
}

update_action_server <- function(id, db, current_user, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    
    
    # --- 1. FETCH REAL ACTIONS ---
    action_data <- reactive({
      db_sync_trigger()
      query <- "
        SELECT DISTINCT
          ia.implementedactionid,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS \"Target\",
          l2.actionl2name AS \"Action\",
          ia.timeframe AS \"Timeframe\",
          ia.status AS \"Status\",
          CASE WHEN ia.createdby = $1::text THEN 'Creator' ELSE 'Delegate' END AS \"Role\"
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1::text OR du.userid = $2::text
      "
      dbGetQuery(db, query, params = list(current_user()$user_id, current_user()$user_id))
    })
    
    output$action_table <- renderDT({
      datatable(action_data(), selection = "single", rownames = FALSE, 
                options = list(pageLength = 5, dom = 'ftp', columnDefs = list(list(visible = FALSE, targets = 0))))
    })
    
    # --- 2. FETCH AND RENDER FULL ACTION DETAILS ---
    output$full_action_details <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      q_info <- "
        SELECT ia.actiondesc, sadd.\"Meaningful.Details\" AS species_detail, hadd.\"Meaningful.Details\" AS habitat_detail
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid
        LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid
        WHERE ia.implementedactionid = $1 LIMIT 1
      "
      info_df <- dbGetQuery(db, q_info, params = list(as.integer(impl_id)))
      
      q_threats <- "
        SELECT l2.threatl2code || '. ' || l2.threatl2name AS threat_name, ta.justification
        FROM track.threatsaddressed ta
        JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid
        JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id
        WHERE sha.implementedactionid = $1
      "
      threats_df <- dbGetQuery(db, q_threats, params = list(as.integer(impl_id)))
      
      desc_text <- if(is.na(info_df$actiondesc[1]) || info_df$actiondesc[1] == "") "No description provided." else info_df$actiondesc[1]
      detail_text <- "None Selected"
      if (!is.na(info_df$species_detail[1])) detail_text <- info_df$species_detail[1]
      if (!is.na(info_df$habitat_detail[1])) detail_text <- info_df$habitat_detail[1]
      
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      threat_ui <- if(nrow(threats_df) > 0) {
        tags$ul(class = "mt-2", lapply(1:nrow(threats_df), function(i) {
          tags$li(strong(threats_df$threat_name[i]), br(), em("Justification: "), threats_df$justification[i])
        }))
      } else { p(em("No threats recorded.")) }
      
      tagList(
        layout_columns(
          div(h6("Core Info", class = "text-muted mb-1"),
              p(strong("Target: "), selected_row$Target, br(), strong("Action: "), selected_row$Action, br(),
                strong("Timeframe: "), selected_row$Timeframe, br(), strong("Status: "), selected_row$Status)),
          div(h6("Action Specifics", class = "text-muted mb-1"),
              p(strong("Lexicon Detail: "), detail_text, br(), strong("User Description: "), desc_text))
        ),
        hr(), h6("Threats Addressed & Justifications", class = "text-muted mb-1"), threat_ui
      )
    })
    
    # --- 2.5 COLLABORATORS & DELEGATION LOGIC (NEW) ---
    output$collaborators_ui <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      # 1. Fetch current people attached to this action
      q_collabs <- "
        SELECT p.first_name || ' ' || p.last_name AS name, 'Creator' AS access_level
        FROM track.implementedactions a JOIN public.profiles p ON a.createdby = p.id::text WHERE a.implementedactionid = $1
        UNION
        SELECT p.first_name || ' ' || p.last_name AS name, 'Delegate' AS access_level
        FROM track.delegateusers d JOIN public.profiles p ON d.userid = p.id::text WHERE d.implementedactionid = $1
        ORDER BY access_level ASC, name ASC
      "
      collabs_df <- dbGetQuery(db, q_collabs, params = list(as.integer(impl_id)))
      
      # Build the visual list of current collaborators
      collab_list <- tags$ul(class = "mt-2", lapply(1:nrow(collabs_df), function(i) {
        badge_color <- if(collabs_df$access_level[i] == "Creator") "bg-primary" else "bg-secondary"
        tags$li(strong(collabs_df$name[i]), span(class = paste("badge ms-2", badge_color), collabs_df$access_level[i]))
      }))
      
      # 2. If the user viewing this IS the creator, give them the tools to add more!
      if (selected_row$Role == "Creator") {
        
        # Query ALL profiles EXCEPT the ones already attached to this action
        q_avail <- "
          SELECT id, first_name || ' ' || last_name AS name FROM public.profiles
          WHERE id::text NOT IN (
            SELECT createdby FROM track.implementedactions WHERE implementedactionid = $1 AND createdby IS NOT NULL
            UNION
            SELECT userid FROM track.delegateusers WHERE implementedactionid = $1 AND userid IS NOT NULL
          ) ORDER BY name ASC
        "
        avail_df <- dbGetQuery(db, q_avail, params = list(as.integer(impl_id)))
        user_choices <- setNames(avail_df$id, avail_df$name)
        
        add_tools <- div(
          h6("Invite a Colleague", class = "fw-bold text-muted"),
          selectInput(ns("new_delegate_id"), NULL, choices = c("Choose a user..." = "", user_choices), width = "100%"),
          actionButton(ns("btn_add_delegate"), "Add Delegate", class = "btn-warning w-100", style = "font-weight: bold;")
        )
        
        layout_columns(
          div(h6("Current Access", class = "fw-bold text-muted"), collab_list), add_tools
        )
      } else {
        # If they are just a delegate, they can only view the list, not add people.
        div(h6("Current Access", class = "fw-bold text-muted"), collab_list)
      }
    })
    
    # Handle the "Add Delegate" button click
    observeEvent(input$btn_add_delegate, {
      req(input$action_table_rows_selected, input$new_delegate_id)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      dbExecute(db, "INSERT INTO track.delegateusers (implementedactionid, userid) VALUES ($1, $2)", 
                params = list(as.integer(impl_id), input$new_delegate_id))
      
      showNotification("Delegate added successfully!", type = "message")
      refresh_trigger(db_sync_trigger() + 1)
    })
    
    # --- 3. FETCH AND RENDER PREVIOUS UPDATES ---
    action_history <- reactive({
      req(input$action_table_rows_selected)
      db_sync_trigger() 
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      query <- "
        SELECT a.actiondate::date AS \"Date\", a.stattype AS \"Metric\", a.stat AS \"Value\", a.comments AS \"Notes\", 
               COALESCE(p.first_name || ' ' || p.last_name, a.createdby) AS \"Entered By\"
        FROM track.actiontracking a LEFT JOIN public.profiles p ON a.createdby = p.id::text
        WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC
      "
      dbGetQuery(db, query, params = list(as.integer(impl_id)))
    })
    
    output$history_table <- renderDT({
      df <- action_history()
      if(nrow(df) == 0) datatable(df, rownames = FALSE, options = list(dom = 't', language = list(emptyTable = "No updates have been recorded for this action yet.")))
      else datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = 'tp'))
    })
    # Auto-update the status dropdown to match the selected action's current status
    observeEvent(input$action_table_rows_selected, {
      req(input$action_table_rows_selected)
      current_status <- action_data()[input$action_table_rows_selected, "Status"]
      updateSelectInput(session, "new_status", selected = current_status)
    })
    # --- 4. SUBMIT NEW UPDATE ---
    observeEvent(input$submit_update, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      tryCatch({
        # 1. Insert the new progress record into track.actiontracking
        q_insert <- "INSERT INTO track.actiontracking (implementedactionid, actiondate, stattype, stat, comments, createdby) VALUES ($1, $2, $3, $4, $5, $6)"
        dbExecute(db, q_insert, params = list(as.integer(selected_row$implementedactionid), as.character(input$action_date), input$stat_type, as.numeric(input$stat_value), input$comments, current_user()$user_id))
        
        # 2. Update the overarching status in track.implementedactions
        q_update_status <- "UPDATE track.implementedactions SET status = $1 WHERE implementedactionid = $2"
        dbExecute(db, q_update_status, params = list(input$new_status, as.integer(selected_row$implementedactionid)))
        
        showNotification("Update successfully recorded and status refreshed!", type = "message", duration = 5)
        
        # Reset the form fields
        updateNumericInput(session, "stat_value", value = 0)
        updateTextAreaInput(session, "comments", value = "")
        
        # Trigger a refresh so the main table updates to show the new status immediately
        refresh_trigger(db_sync_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    })
  })
}