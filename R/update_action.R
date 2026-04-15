update_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Update an Existing Action", class = "mb-4"),
    p("Select an action from the list on the left to view its full details, log new progress, or manage collaborators.", class = "text-muted mb-4"),
    
    fluidRow(
      column(5,
             div(class = "card mb-4 shadow-sm",
                 div(class = "card-header text-white fw-bold", style = "background-color: #07234C;", "Your Actions & Delegations"),
                 div(class = "card-body",
                     DTOutput(ns("action_table"))
                 )
             )
      ),
      
      column(7,
            
             conditionalPanel(
               condition = sprintf("input['%s'] != null", ns("action_table_rows_selected")),
               
               div(class = "card shadow-sm", style = "border: 1px solid #dee2e6;",
                   div(class = "card-body p-4",
                       
                       navset_underline(
                         id = ns("update_tabs"),
                         
                         #  TAB 1: ACTION INFORMATION
                         nav_panel("Action Information",
                                   div(class = "mt-4",
                                       
                                       # FULL ACTION DETAILS
                                       div(class = "card mb-4 shadow-sm", style = "border-color: #07234C;",
                                           div(class = "card-header text-white", style = "background-color: #07234C;", "Details"),
                                           div(class = "card-body bg-light",
                                               uiOutput(ns("full_action_details"))
                                           )
                                       ),
                                       
                                       # COLLABORATORS & DELEGATION
                                       div(class = "card mb-2 shadow-sm", style = "border-color: #AA5F40;",
                                           div(class = "card-header text-white", style = "background-color: #AA5F40;", "Action Permissions"),
                                           div(class = "card-body",
                                               uiOutput(ns("collaborators_ui"))
                                           )
                                       )
                                   )
                         ),
                         
                         #TAB 2: LOG PROGRESS
                         nav_panel("Log Update/Progress",
                                   div(class = "mt-4",
     
                                       div(class = "card mb-4 shadow-sm",
                                           div(class = "card-header text-white", style = "background-color: #055A53;", "Progress Logs"),
                                           div(class = "card-body",
                                               DTOutput(ns("history_table"))
                                           )
                                       ),
                                       
                                       div(class = "card mb-2 shadow-sm", style = "border-color: #0D67B8;",
                                           div(class = "card-header text-white", style = "background-color: #0D67B8;", "Log Update/Progress"),
                                           div(class = "card-body", style = "overflow: visible;",
                                               p("Submit updates/progress metrics (e.g., acres treated, surveys completed, percentage of plan completed, etc.).", class="text-muted"),
                                               layout_columns(
                                                 dateInput(ns("action_date"), "Date of Update", value = Sys.Date(), width = "100%"),
                                                 selectInput(ns("stat_type"), "Metric (% or Count)", choices = c("Percentage", "Count"), width = "100%"),
                                                 numericInput(ns("stat_value"), "Value", value = 0, min = 0, width = "100%")
                                               ),
                                               div(class = "mt-3",
                                                   textAreaInput(ns("comments"), "Update Notes", rows = 2, width = "100%")
                                               ),
                                               actionButton(ns("submit_progress"), "Submit", class = "btn-primary btn-lg mt-3 w-100", style="font-weight: bold;")
                                           )
                                       )
                                   )
                         ),
                         
                         # TAB 3: UPDATE OVERALL ACTION STATUS
                         nav_panel("Update Overall Action Status",
                                   div(class = "mt-4",
                                       
                                       # Action Summary Context Box
                                       uiOutput(ns("status_action_summary")),
                                       
                                       div(class = "card mb-2 shadow-sm", style = "border-color: #EAB11E;",
                                           div(class = "card-header text-dark fw-bold", style = "background-color: #EAB11E;", "Overall Action Lifecycle"),
                                           div(class = "card-body",
                                               p("Change this only if the entire action has moved to a new phase (e.g., transitioning from Planned to In Progress, or marking the project as fully Completed).", class="text-muted"),
                                               layout_columns(
                                                 selectInput(ns("new_status"), "Current Status", 
                                                             choices = c("Planned", "In Progress", "Completed"), 
                                                             width = "100%"),
                                                 actionButton(ns("submit_status"), "Update Overall Status", class = "btn-success btn-lg mt-3 w-100", style="font-weight: bold;")
                                               )
                                           )
                                       )
                                   )
                         )
                       )
                   )
               )
             )
      )
    )
  )
}

update_action_server <- function(id, db, current_user, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    
    # --- FETCH ACTIONS ---
    action_data <- reactive({
      db_sync_trigger()
      # THE FIX: We use STRING_AGG to combine targets into a single hidden column
      query <- "
        SELECT 
          ia.implementedactionid,
          l2.actionl2code || '. ' || l2.actionl2name AS \"Action\",
          ia.timeframe AS \"Timeframe\",
          ia.status AS \"Status\",
          CASE WHEN ia.createdby = $1::text THEN 'Creator' ELSE 'Delegate' END AS \"Role\",
          STRING_AGG(CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END, ', ') AS \"Searchable_Targets\"
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1::text OR du.userid = $2::text
        GROUP BY ia.implementedactionid, l2.actionl2code, l2.actionl2name, ia.timeframe, ia.status, ia.createdby
        ORDER BY ia.status DESC, \"Action\" ASC
      "
      dbGetQuery(db, query, params = list(current_user()$user_id, current_user()$user_id))
    })
    
    output$action_table <- renderDT({
      datatable(isolate(action_data()), selection = "single", rownames = FALSE, 
                options = list(
                  scrollY = "500px", 
                  paging = FALSE, 
                  dom = 'ft',
                  scrollCollapse = TRUE,
                  # THE FIX: Hide index 0 (ID) and index 5 (Searchable_Targets), but keep them searchable!
                  columnDefs = list(list(visible = FALSE, targets = c(0, 5)))
                ))
    })
    
    proxy_action_table <- dataTableProxy("action_table")
    observeEvent(action_data(), {
      replaceData(proxy_action_table, action_data(), resetPaging = FALSE, clearSelection = "none", rownames = FALSE)
    }, ignoreInit = TRUE)
    
    # --- FETCH AND RENDER ACTION DETAILS ---
    output$full_action_details <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      # 1. Fetch Core Info (Description)
      q_core <- "SELECT actiondesc FROM track.implementedactions WHERE implementedactionid = $1"
      core_df <- dbGetQuery(db, q_core, params = list(as.integer(impl_id)))
      desc_text <- if(nrow(core_df) > 0 && !is.na(core_df$actiondesc[1]) && core_df$actiondesc[1] != "") core_df$actiondesc[1] else "No description provided."
      
      # 2. Fetch All Targets & Lexicon Details for this Action
      q_targ <- "
        SELECT 
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name,
          CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type,
          COALESCE(sadd.\"Meaningful.Details\", hadd.\"Meaningful.Details\", 'None Selected') AS detail
        FROM track.specieshabitatactions sha
        LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid
        LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        WHERE sha.implementedactionid = $1
      "
      targ_df <- dbGetQuery(db, q_targ, params = list(as.integer(impl_id)))
      
      # 3. Fetch All Distinct Threats for this Action WITH their associated Targets
      q_threats <- "
        SELECT 
          l2.threatl2code || '. ' || l2.threatl2name AS threat_name, 
          ta.justification,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name
        FROM track.threatsaddressed ta
        JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid
        JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        WHERE sha.implementedactionid = $1
        ORDER BY target_name ASC, threat_name ASC
      "
      threats_df <- dbGetQuery(db, q_threats, params = list(as.integer(impl_id)))
      
      # Build HTML for Targets
      targ_ui <- if(nrow(targ_df) > 0) {
        tags$ul(class = "mt-2 mb-0", lapply(1:nrow(targ_df), function(i) {
          tags$li(strong(targ_df$target_name[i]), " (", targ_df$target_type[i], ")", br(),
                  em("Lexicon Detail: "), targ_df$detail[i], class="mb-2")
        }))
      } else { p(em("No targets assigned."), class="mb-0") }
      
      # Build HTML for Threats (Now including Target labels!)
      threat_ui <- if(nrow(threats_df) > 0) {
        tags$ul(class = "mt-2 mb-0", lapply(1:nrow(threats_df), function(i) {
          tags$li(
            strong(threats_df$threat_name[i]), 
            span(class = "text-primary", style = "font-size: 0.9em; font-weight: bold;", paste0(" [", threats_df$target_name[i], "]")),
            br(), 
            em("Justification: "), threats_df$justification[i], 
            class = "mb-3"
          )
        }))
      } else { p(em("No threats recorded."), class="mb-0") }
      
      tagList(
        layout_columns(
          div(h6("Action Overview", class = "text-muted mb-1"),
              p(strong("Action: "), selected_row$Action, br(),
                strong("Timeframe: "), selected_row$Timeframe, br(),
                strong("Status: "), selected_row$Status)),
          div(h6("Implementation Specifics", class = "text-muted mb-1"),
              p(strong("User Description: "), desc_text))
        ),
        hr(), 
        layout_columns(
          div(h6("Targets & Details", class = "text-muted mb-1"), targ_ui),
          div(h6("Mitigated Threats", class = "text-muted mb-1"), threat_ui)
        )
      )
    })
    
    # COLLABORATORS & DELEGATION LOGIC 
    output$collaborators_ui <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      q_collabs <- "
        SELECT p.first_name || ' ' || p.last_name AS name, 'Creator' AS access_level
        FROM track.implementedactions a JOIN public.profiles p ON a.createdby = p.id::text WHERE a.implementedactionid = $1
        UNION
        SELECT p.first_name || ' ' || p.last_name AS name, 'Delegate' AS access_level
        FROM track.delegateusers d JOIN public.profiles p ON d.userid = p.id::text WHERE d.implementedactionid = $1
        ORDER BY access_level ASC, name ASC
      "
      collabs_df <- dbGetQuery(db, q_collabs, params = list(as.integer(impl_id)))
      
      collab_list <- tags$ul(class = "mt-2", lapply(1:nrow(collabs_df), function(i) {
        badge_color <- if(collabs_df$access_level[i] == "Creator") "bg-primary" else "bg-secondary"
        tags$li(strong(collabs_df$name[i]), span(class = paste("badge ms-2", badge_color), collabs_df$access_level[i]))
      }))
      
      if (selected_row$Role == "Creator") {
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
          h6("Delegate a Colleague", class = "fw-bold text-muted"),
          selectInput(session$ns("new_delegate_id"), NULL, choices = c("Choose a user..." = "", user_choices), width = "100%"),
          actionButton(session$ns("btn_add_delegate"), "Add Delegate", class = "btn-warning w-100", style = "font-weight: bold;")
        )
        
        layout_columns(div(h6("Current Access", class = "fw-bold text-muted"), collab_list), add_tools)
      } else {
        div(h6("Current Access", class = "fw-bold text-muted"), collab_list)
      }
    })
    
    observeEvent(input$btn_add_delegate, {
      req(input$action_table_rows_selected, input$new_delegate_id)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      dbExecute(db, "INSERT INTO track.delegateusers (implementedactionid, userid) VALUES ($1, $2)", 
                params = list(as.integer(impl_id), input$new_delegate_id))
      
      showNotification("Delegate added successfully!", type = "message")
      db_sync_trigger(db_sync_trigger() + 1)
    })
    
    # PREVIOUS UPDATES HISTORY 
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
    
    # Lock in the Metric Type dropdown (percentage or count) if history exists
    observeEvent(action_history(), {
      df <- action_history()
      if (nrow(df) > 0) {
        locked_metric <- df$Metric[1]
        updateSelectInput(session, "stat_type", choices = locked_metric, selected = locked_metric)
      } else {
        updateSelectInput(session, "stat_type", choices = c("Percentage", "Count"), selected = "Percentage")
      }
    })
    
    output$history_table <- renderDT({
      df <- action_history()
      if(nrow(df) == 0) datatable(df, rownames = FALSE, options = list(
        dom = 't',
        paging = FALSE,        
        scrollY = "200px", 
        scrollCollapse = TRUE,
        language = list(emptyTable = "No updates have been recorded for this action yet.")))
      else datatable(df, rownames = FALSE, options = list(dom = 't',
                                                          paging = FALSE,        
                                                          scrollY = "200px",
                                                          scrollCollapse = TRUE))
    })
    
    observeEvent(input$action_table_rows_selected, {
      req(input$action_table_rows_selected)
      current_status <- action_data()[input$action_table_rows_selected, "Status"]
      updateSelectInput(session, "new_status", selected = current_status)
    })
    
    # SUBMIT LOG PROGRESS 
    
    execute_progress_log <- function(selected_row) {
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          q_insert <- "INSERT INTO track.actiontracking (implementedactionid, actiondate, stattype, stat, comments, createdby) VALUES ($1, $2, $3, $4, $5, $6)"
          dbExecute(conn, q_insert, params = list(as.integer(selected_row$implementedactionid), as.character(input$action_date), input$stat_type, as.numeric(input$stat_value), input$comments, current_user()$user_id))
        })
        
        showNotification("Update/Progress successfully recorded!", type = "message", duration = 5)
        
        updateDateInput(session, "action_date", value = Sys.Date())
        updateNumericInput(session, "stat_value", value = 0)
        updateTextAreaInput(session, "comments", value = "")
        bslib::nav_select("update_tabs", "Action Information", session = session)
        
        db_sync_trigger(db_sync_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    }
    
    observeEvent(input$submit_progress, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      hist_df <- action_history()
      
      warning_msg <- ""
      
      if (nrow(hist_df) > 0) {
        last_date <- as.Date(hist_df$Date[1]) 
        last_val <- as.numeric(hist_df$Value[1])
        new_date <- as.Date(input$action_date)
        new_val <- as.numeric(input$stat_value)
        
        if (new_date <= last_date) {
          warning_msg <- paste0(warning_msg, "<li>You are logging an update on <b>", new_date, "</b>, which is the same as or prior to the most recent logged date (<b>", last_date, "</b>).</li>")
        }
        if (input$stat_type == "Percentage" && new_date > last_date && new_val < last_val) {
          warning_msg <- paste0(warning_msg, "<li>You are logging a percentage (<b>", new_val, "%</b>) that is <i>lower</i> than the previously logged percentage (<b>", last_val, "%</b>).</li>")
        }
      }
      
      if (warning_msg != "") {
        showModal(modalDialog(
          title = "Please Confirm Your Update",
          HTML(paste0("<p>We noticed a potential discrepancy with your update/progress log:</p><ul>", warning_msg, "</ul><p>Are you sure you want to proceed and save this data?</p>")),
          footer = tagList(
            tagAppendAttributes(modalButton("Cancel"), style = "color: #333; background-color: #e9ecef; border-color: #ccc;"),
            actionButton(session$ns("confirm_progress_warning"), "Confirm & Save", class = "btn-warning", style="font-weight: bold;")
          )
        ))
      } else {
        # If no warnings, save instantly
        execute_progress_log(selected_row)
      }
    })
    
    observeEvent(input$confirm_progress_warning, {
      removeModal()
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      execute_progress_log(selected_row)
    })
    
    # --- ACTION SUMMARY FOR STATUS TAB ---
    output$status_action_summary <- renderUI({
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      div(class = "alert alert-secondary shadow-sm mb-4", style = "border-left: 5px solid #07234C;",
          h6("You are updating the status for:", class = "alert-heading fw-bold mb-2"),
          p(class = "mb-1", strong("Action: "), selected_row$Action),
          p(class = "mb-1", strong("Applied to Targets: "), selected_row$Searchable_Targets),
          p(class = "mb-0", strong("Timeframe: "), selected_row$Timeframe)
      )
    })
    
    # SUBMIT STATUS CHANGE
    observeEvent(input$submit_status, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      if (input$new_status == selected_row$Status) {
        showNotification("The action is already set to this status.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Confirm Status Change",
        p(HTML(paste0("You are about to change the overall status of this action from <b>", selected_row$Status, "</b> to <b>", input$new_status, "</b>."))),
        p("Are you sure you want to proceed? This will update the status for all targets associated with this overarching action."),
        footer = tagList(
          tagAppendAttributes(modalButton("Cancel"), style = "color: #333; background-color: #e9ecef; border-color: #ccc;"),
          actionButton(session$ns("confirm_status_change"), "Confirm & Save", class = "btn-warning", style="font-weight: bold;")
        )
      ))
    })
    
    observeEvent(input$confirm_status_change, {
      removeModal()
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          q_update_status <- "UPDATE track.implementedactions SET status = $1 WHERE implementedactionid = $2"
          dbExecute(conn, q_update_status, params = list(input$new_status, as.integer(selected_row$implementedactionid)))
        })
        
        showNotification("Overall status successfully updated!", type = "message", duration = 5)
        bslib::nav_select("update_tabs", "Action Information", session = session)
        db_sync_trigger(db_sync_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}