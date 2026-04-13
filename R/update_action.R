update_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Update an Existing Action", class = "mb-4"),
    p("Select an action below to view its full details, log new progress, or manage collaborators."),
    
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
      
      navset_underline(
        id = ns("update_tabs"),
        
        # ==========================================
        # TAB 1: ACTION INFORMATION
        # ==========================================
        nav_panel("Action Information",
                  div(class = "mt-3",
                      
                      # FULL ACTION DETAILS
                      div(class = "card mb-4 shadow-sm", style = "border-color: #07234C;",
                          div(class = "card-header text-white", style = "background-color: #07234C;", "Target & Action Specifics"),
                          div(class = "card-body bg-light",
                              uiOutput(ns("full_action_details"))
                          )
                      ),
                      
                      # COLLABORATORS & DELEGATION
                      div(class = "card mb-4 shadow-sm", style = "border-color: #AA5F40;",
                          div(class = "card-header text-white", style = "background-color: #AA5F40;", "Action Collaborators"),
                          div(class = "card-body",
                              uiOutput(ns("collaborators_ui"))
                          )
                      )
                  )
        ),
        
        # ==========================================
        # TAB 2: LOG PROGRESS
        # ==========================================
        nav_panel("Log Progress",
                  div(class = "mt-3",
                      
                      # PREVIOUS UPDATES HISTORY (Now on top!)
                      div(class = "card mb-4 shadow-sm",
                          div(class = "card-header text-white", style = "background-color: #055A53;", "Previous Updates History"),
                          div(class = "card-body",
                              DTOutput(ns("history_table"))
                          )
                      ),
                      
                      # ADD NEW UPDATE ENTRY (Now on the bottom!)
                      div(class = "card mb-4 shadow-sm", style = "border-color: #0D67B8;",
                          div(class = "card-header text-white", style = "background-color: #0D67B8;", "Log Incremental Progress"),
                          div(class = "card-body", style = "overflow: visible;",
                              p("Use this section to log daily or seasonal progress metrics (e.g., acres treated, surveys completed).", class="text-muted"),
                              layout_columns(
                                dateInput(ns("action_date"), "Date of Update", value = Sys.Date(), width = "100%"),
                                selectInput(ns("stat_type"), "Metric Type", choices = c("Percentage", "Count"), width = "100%"),
                                numericInput(ns("stat_value"), "Value", value = 0, min = 0, width = "100%")
                              ),
                              div(class = "mt-3",
                                  textAreaInput(ns("comments"), "Update Notes", rows = 2, width = "100%")
                              ),
                              actionButton(ns("submit_progress"), "Save Progress Log", class = "btn-primary btn-lg mt-3 w-100", style="font-weight: bold;")
                          )
                      )
                  )
        ),
        
        # ==========================================
        # TAB 3: UPDATE OVERALL ACTION STATUS
        # ==========================================
        nav_panel("Update Overall Action Status",
                  div(class = "mt-3",
                      
                      # NEW: Action Summary Context Box
                      uiOutput(ns("status_action_summary")),
                      
                      div(class = "card mb-5 shadow-sm", style = "border-color: #EAB11E;",
                          div(class = "card-header text-dark fw-bold", style = "background-color: #EAB11E;", "Overall Action Lifecycle"),
                          div(class = "card-body",
                              p("Change this only if the entire action has moved to a new phase (e.g., transitioning from Planned to In Progress, or marking the project as fully Completed).", class="text-muted"),
                              layout_columns(
                                selectInput(ns("new_status"), "Current Status", 
                                            choices = c("Planned", "In Progress", "Completed"), 
                                            width = "100%"),
                                actionButton(ns("submit_status"), "Change Overall Status", class = "btn-success btn-lg mt-3 w-100", style="font-weight: bold;")
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
    
    # --- 1. FETCH REAL ACTIONS ---
    action_data <- reactive({
      db_sync_trigger()
      query <- "
        SELECT DISTINCT
          ia.implementedactionid,
          sha.specieshabitatactionsid,
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
        ORDER BY ia.status DESC, \"Target\" ASC
      "
      dbGetQuery(db, query, params = list(current_user()$user_id, current_user()$user_id))
    })
    
    # THE FIX: Isolate the initial render and apply scrolling
    output$action_table <- renderDT({
      datatable(isolate(action_data()), selection = "single", rownames = FALSE, 
                options = list(scrollY = "300px", paging = FALSE, dom = 'ft', columnDefs = list(list(visible = FALSE, targets = c(0, 1)))))
    })
    
    # THE FIX: Use dataTableProxy to refresh data without losing selection or scroll position
    proxy_action_table <- dataTableProxy("action_table")
    observeEvent(action_data(), {
      replaceData(proxy_action_table, action_data(), resetPaging = FALSE, clearSelection = "none", rownames = FALSE)
    }, ignoreInit = TRUE)
    
    # --- 2. FETCH AND RENDER FULL ACTION DETAILS ---
    output$full_action_details <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      sha_id <- action_data()[input$action_table_rows_selected, "specieshabitatactionsid"]
      
      q_info <- "
        SELECT ia.actiondesc, sadd.\"Meaningful.Details\" AS species_detail, hadd.\"Meaningful.Details\" AS habitat_detail
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid
        LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid
        WHERE sha.specieshabitatactionsid = $1 LIMIT 1
      "
      info_df <- dbGetQuery(db, q_info, params = list(as.integer(sha_id)))
      
      q_threats <- "
        SELECT l2.threatl2code || '. ' || l2.threatl2name AS threat_name, ta.justification
        FROM track.threatsaddressed ta
        JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid
        JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id
        WHERE sha.specieshabitatactionsid = $1
      "
      threats_df <- dbGetQuery(db, q_threats, params = list(as.integer(sha_id)))
      
      q_others <- "
        SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, 
               CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type 
        FROM track.specieshabitatactions sha 
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid 
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid 
        WHERE sha.implementedactionid = $1 AND sha.specieshabitatactionsid != $2
      "
      others_df <- dbGetQuery(db, q_others, params = list(as.integer(impl_id), as.integer(sha_id)))
      
      desc_text <- if(is.na(info_df$actiondesc[1]) || info_df$actiondesc[1] == "") "No description provided." else info_df$actiondesc[1]
      detail_text <- "None Selected"
      if (!is.na(info_df$species_detail[1])) detail_text <- info_df$species_detail[1]
      if (!is.na(info_df$habitat_detail[1])) detail_text <- info_df$habitat_detail[1]
      
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      threat_ui <- if(nrow(threats_df) > 0) {
        tags$ul(class = "mt-2 mb-0", lapply(1:nrow(threats_df), function(i) {
          tags$li(strong(threats_df$threat_name[i]), br(), em("Justification: "), threats_df$justification[i], class = "mb-2")
        }))
      } else { p(em("No threats recorded."), class="mb-0") }
      
      others_ui <- if(nrow(others_df) > 0) {
        tags$ul(class = "mt-2 mb-0", lapply(1:nrow(others_df), function(i) {
          tags$li(strong(others_df$target_name[i]), " (", others_df$target_type[i], ")")
        }))
      } else { p(em("This action applies exclusively to this target."), class="mb-0") }
      
      tagList(
        layout_columns(
          div(h6("Core Info", class = "text-muted mb-1"),
              p(strong("Target: "), selected_row$Target, br(), strong("Action: "), selected_row$Action, br(),
                strong("Timeframe: "), selected_row$Timeframe, br(), strong("Status: "), selected_row$Status)),
          div(h6("Action Specifics", class = "text-muted mb-1"),
              p(strong("Lexicon Detail: "), detail_text, br(), strong("User Description: "), desc_text))
        ),
        hr(), 
        layout_columns(
          div(h6("Mitigated Threats for this Target", class = "text-muted mb-1"), threat_ui),
          div(h6("Other Targets in this Action", class = "text-muted mb-1"), others_ui)
        )
      )
    })
    
    # --- 3. COLLABORATORS & DELEGATION LOGIC ---
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
          h6("Invite a Colleague", class = "fw-bold text-muted"),
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
    
    # --- 4. PREVIOUS UPDATES HISTORY & METRIC LOCK-IN ---
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
    
    # THE FIX: Lock in the Metric Type dropdown if history exists
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
      if(nrow(df) == 0) datatable(df, rownames = FALSE, options = list(dom = 't', language = list(emptyTable = "No updates have been recorded for this action yet.")))
      else datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = 'tp'))
    })
    
    observeEvent(input$action_table_rows_selected, {
      req(input$action_table_rows_selected)
      current_status <- action_data()[input$action_table_rows_selected, "Status"]
      updateSelectInput(session, "new_status", selected = current_status)
    })
    
    # --- 5. SUBMIT LOG PROGRESS (TAB 2) ---
    
    # Helper to execute the progress save so we don't write it twice
    execute_progress_log <- function(selected_row) {
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          q_insert <- "INSERT INTO track.actiontracking (implementedactionid, actiondate, stattype, stat, comments, createdby) VALUES ($1, $2, $3, $4, $5, $6)"
          dbExecute(conn, q_insert, params = list(as.integer(selected_row$implementedactionid), as.character(input$action_date), input$stat_type, as.numeric(input$stat_value), input$comments, current_user()$user_id))
        })
        
        showNotification("Progress log successfully recorded!", type = "message", duration = 5)
        
        # THE FIX: Reset form fields, keep the user on the action, and swap back to info tab
        updateDateInput(session, "action_date", value = Sys.Date())
        updateNumericInput(session, "stat_value", value = 0)
        updateTextAreaInput(session, "comments", value = "")
        bslib::nav_select("update_tabs", "Action Information", session = session)
        
        # Trigger global refresh (replaceData safely preserves selection)
        db_sync_trigger(db_sync_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    }
    
    # THE FIX: Smart Validation Warnings
    observeEvent(input$submit_progress, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      hist_df <- action_history()
      
      warning_msg <- ""
      
      if (nrow(hist_df) > 0) {
        # hist_df is ordered by actiondate DESC, so row 1 is the most recent chronological entry
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
          HTML(paste0("<p>We noticed a potential discrepancy with your progress log:</p><ul>", warning_msg, "</ul><p>Are you sure you want to proceed and save this data?</p>")),
          footer = tagList(
            modalButton("Cancel"),
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
    
    # --- 5.5 ACTION SUMMARY FOR STATUS TAB ---
    output$status_action_summary <- renderUI({
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      sha_id <- selected_row$specieshabitatactionsid
      
      # Fetch the Action Detail if it exists
      q_info <- "
        SELECT sadd.\"Meaningful.Details\" AS species_detail, hadd.\"Meaningful.Details\" AS habitat_detail
        FROM track.specieshabitatactions sha
        LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid
        LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid
        WHERE sha.specieshabitatactionsid = $1 LIMIT 1
      "
      info_df <- dbGetQuery(db, q_info, params = list(as.integer(sha_id)))
      
      detail_text <- "None"
      if (nrow(info_df) > 0) {
        if (!is.na(info_df$species_detail[1])) detail_text <- info_df$species_detail[1]
        if (!is.na(info_df$habitat_detail[1])) detail_text <- info_df$habitat_detail[1]
      }
      
      # Build a clean alert box to display the context
      div(class = "alert alert-secondary shadow-sm mb-4", style = "border-left: 5px solid #07234C;",
          h6("You are updating the status for:", class = "alert-heading fw-bold mb-2"),
          layout_columns(
            div(
              p(class = "mb-1", strong("Target: "), selected_row$Target),
              p(class = "mb-0", strong("L2 Action: "), selected_row$Action)
            ),
            div(
              p(class = "mb-1", strong("Action Detail: "), detail_text),
              p(class = "mb-0", strong("Timeframe: "), selected_row$Timeframe)
            )
          )
      )
    })
    
    # --- 6. SUBMIT STATUS CHANGE (TAB 3) ---
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
          modalButton("Cancel"),
          actionButton(session$ns("confirm_status_change"), "Confirm & Save", class = "btn-warning", style="font-weight: bold;")
        )
      ))
    })
    
    # Execute the status change after modal confirmation
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
        # Shift user back to core info tab
        bslib::nav_select("update_tabs", "Action Information", session = session)
        db_sync_trigger(db_sync_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}