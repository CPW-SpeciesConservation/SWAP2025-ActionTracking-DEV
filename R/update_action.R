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
                         
                         # TAB 2: LOG PROGRESS & NARRATIVE
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
                                               p("Update the current progress statuses and provide narrative context for this action.", class="text-muted"),
                                               
                                               layout_columns(
                                                 dateInput(ns("action_date"), "Date of Update", value = Sys.Date(), width = "100%"),
                                                 selectInput(ns("upd_impl_prog"), "Implementation Progress", 
                                                             choices = c("Not specified", "Scheduled for future", "Major issues", "Minor issues", "On-track", "Completed", "Abandoned")),
                                                 selectInput(ns("upd_res_prog"), "Effectiveness Progress", 
                                                             choices = c("Not specified", "Not Yet", "Not achieved", "Partially achieved", "On-track", "Achieved", "No longer relevant"))
                                               ),
                                               
                                               # Narrative Text Boxes
                                               div(class = "mt-2",
                                                   textAreaInput(ns("what_done"), "1. What was done?", rows = 2, width = "100%", placeholder = "Describe the specific activities carried out..."),
                                                   textAreaInput(ns("what_learned"), "2. What was learned?", rows = 2, width = "100%", placeholder = "Describe how effective the action was and any new insights..."),
                                                   textAreaInput(ns("what_needed"), "3. What is still needed?", rows = 2, width = "100%", placeholder = "Describe next steps, funding needs, or strategy shifts...")
                                               ),
                                               
                                               hr(),
                                               
                                               # Optional Resource Upload
                                               checkboxInput(ns("add_resource_check"), strong("I have a file/resource to upload with this update"), value = FALSE),
                                               conditionalPanel(
                                                 condition = sprintf("input['%s'] == true", ns("add_resource_check")),
                                                 div(class = "p-3 rounded mt-2 mb-3", style = "background-color: #F8F9FA; border: 1px solid #DEE2E6;",
                                                     h6("Upload Resource", class="fw-bold text-muted"),
                                                     selectInput(ns("res_target"), "Apply this resource to which target?", choices = c("Loading..." = ""), width = "100%"),
                                                     layout_columns(
                                                       textInput(ns("res_name"), "Resource Name", placeholder = "e.g., 2025 Survey Report"),
                                                       selectInput(ns("res_type"), "Resource Type", choices = c(
                                                         "Document/Report" = "Document", 
                                                         "Data/Spreadsheet" = "Data", 
                                                         "Map/GIS" = "Map", 
                                                         "Management Plan" = "Plan", 
                                                         "Other" = "Other"
                                                       ))
                                                     ),
                                                     fileInput(ns("res_file"), "Select File (PDF, Word, Excel, CSV)", accept = c(".pdf", ".doc", ".docx", ".xls", ".xlsx", ".csv"))
                                                 )
                                               ),
                                               
                                               actionButton(ns("submit_progress"), "Submit Update", class = "btn-primary btn-lg mt-3 w-100", style="font-weight: bold;")
                                           )
                                       )
                                   )
                         )
                         # NOTE: Tab 3 (Overall Status) was removed because the Stoplight statuses replace it!
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
      query <- "
        SELECT 
          ia.implementedactionid,
          l2.actionl2code || '. ' || l2.actionl2name AS \"Action\",
          ia.timeframe AS \"Timeframe\",
          ia.implementation_progress AS \"Implementation\",
          ia.result_progress AS \"Effectiveness\",
          CASE WHEN ia.createdby = $1::text THEN 'Creator' ELSE 'Delegate' END AS \"Role\",
          STRING_AGG(CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END, ', ') AS \"Searchable_Targets\"
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1::text OR du.userid = $2::text
        GROUP BY ia.implementedactionid, l2.actionl2code, l2.actionl2name, ia.timeframe, ia.implementation_progress, ia.result_progress, ia.createdby
        ORDER BY ia.implementation_progress ASC, \"Action\" ASC
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
                  # HIDE: index 0 (ID) and index 6 (Searchable_Targets)
                  columnDefs = list(list(visible = FALSE, targets = c(0, 6)))
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
      
      q_core <- "SELECT actiondesc FROM track.implementedactions WHERE implementedactionid = $1"
      core_df <- dbGetQuery(db, q_core, params = list(as.integer(impl_id)))
      desc_text <- if(nrow(core_df) > 0 && !is.na(core_df$actiondesc[1]) && core_df$actiondesc[1] != "") core_df$actiondesc[1] else "No description provided."
      
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
      
      q_threats <- "
        SELECT 
          l2.threatl2code || '. ' || l2.threatl2name AS threat_name, 
          ta.justification, ta.alternative_category, ta.justification_text,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, ta.threatl2id
        FROM track.threatsaddressed ta
        JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid
        JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        WHERE sha.implementedactionid = $1
        ORDER BY target_name ASC, threat_name ASC
      "
      threats_df <- dbGetQuery(db, q_threats, params = list(as.integer(impl_id)))
      
      targ_ui <- if(nrow(targ_df) > 0) {
        tags$ul(class = "mt-2 mb-0", lapply(1:nrow(targ_df), function(i) {
          tags$li(strong(targ_df$target_name[i]), " (", targ_df$target_type[i], ")", br(),
                  em("Lexicon Detail: "), targ_df$detail[i], class="mb-2")
        }))
      } else { p(em("No targets assigned."), class="mb-0") }
      
      threat_ui <- if(nrow(threats_df) > 0) {
        tags$ul(class = "mt-2 mb-0", lapply(1:nrow(threats_df), function(i) {
          if (threats_df$threatl2id[i] == 59) {
            tags$li(strong("Broader Conservation Goal: ", style="color: #0D67B8;"), threats_df$alternative_category[i], 
                    span(class = "text-primary", style = "font-size: 0.9em; font-weight: bold;", paste0(" [", threats_df$target_name[i], "]")),
                    br(), em("Justification: "), threats_df$justification_text[i], class = "mb-3")
          } else {
            tags$li(strong(threats_df$threat_name[i]), 
                    span(class = "text-primary", style = "font-size: 0.9em; font-weight: bold;", paste0(" [", threats_df$target_name[i], "]")),
                    br(), em("Justification: "), threats_df$justification[i], class = "mb-3")
          }
        }))
      } else { p(em("No threats recorded."), class="mb-0") }
      
      tagList(
        layout_columns(
          div(h6("Action Overview", class = "text-muted mb-1"),
              p(strong("Action: "), selected_row$Action, br(),
                strong("Timeframe: "), selected_row$Timeframe, br(),
                strong("Impl. Progress: "), selected_row$Implementation, br(),
                strong("Effectiveness: "), selected_row$Effectiveness)),
          div(h6("Implementation Specifics", class = "text-muted mb-1"),
              p(strong("User Description: "), desc_text))
        ),
        hr(), 
        layout_columns(
          div(h6("Targets & Details", class = "text-muted mb-1"), targ_ui),
          div(h6("Mitigated Threats / Goals", class = "text-muted mb-1"), threat_ui)
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
    
    # --- NARRATIVE PROGRESS HISTORY --- 
    action_history <- reactive({
      req(input$action_table_rows_selected)
      db_sync_trigger() 
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      query <- "
        SELECT a.actiondate::date AS \"Date\", 
               a.implementation_progress AS \"Impl. Progress\", 
               a.result_progress AS \"Effectiveness\", 
               a.what_done AS \"What was done?\",
               a.what_learned AS \"What was learned?\",
               a.what_needed AS \"What is needed?\",
               COALESCE(p.first_name || ' ' || p.last_name, a.createdby) AS \"Entered By\"
        FROM track.actiontracking a LEFT JOIN public.profiles p ON a.createdby = p.id::text
        WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC
      "
      dbGetQuery(db, query, params = list(as.integer(impl_id)))
    })
    
    # Pre-fill dropdowns and resource targets when row is selected
    observeEvent(input$action_table_rows_selected, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      updateSelectInput(session, "upd_impl_prog", selected = selected_row$Implementation)
      updateSelectInput(session, "upd_res_prog", selected = selected_row$Effectiveness)
      
      # Populate the dynamic Resource Target dropdown based on this action's targets
      q_targs <- "
        SELECT 
          CASE WHEN sha.specieshabitat = TRUE THEN 'sp_' || sha.speciesid ELSE 'hab_' || sha.habitatsubtypeid END AS id_val,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS label_val
        FROM track.specieshabitatactions sha
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        WHERE sha.implementedactionid = $1
        ORDER BY label_val ASC
      "
      targs_df <- dbGetQuery(db, q_targs, params = list(as.integer(selected_row$implementedactionid)))
      updateSelectInput(session, "res_target", choices = c("Select target..." = "", setNames(targs_df$id_val, targs_df$label_val)))
    })
    
    output$history_table <- renderDT({
      df <- action_history()
      if(nrow(df) == 0) datatable(df, rownames = FALSE, options = list(
        dom = 't', paging = FALSE, scrollY = "200px", scrollCollapse = TRUE,
        language = list(emptyTable = "No updates have been recorded for this action yet.")))
      else datatable(df, rownames = FALSE, options = list(dom = 't', paging = FALSE, scrollY = "300px", scrollCollapse = TRUE))
    })
    
    # --- SUBMIT NARRATIVE PROGRESS LOG ---
    execute_progress_log <- function(selected_row) {
      
      # 1. Handle File Upload (If checked)
      if (isTRUE(input$add_resource_check)) {
        if (is.null(input$res_file) || input$res_target == "" || input$res_name == "") {
          showNotification("Please fill out all resource fields and select a file.", type = "error")
          return()
        }
        
        base_url <- Sys.getenv("SUPABASE_URL")
        api_key <- Sys.getenv("SUPABASE_ANON_KEY")
        user_token <- current_user()$token
        
        safe_filename <- paste0(as.integer(Sys.time()), "_", gsub("[^[:alnum:]._-]", "", input$res_file$name))
        file_ext <- tolower(tools::file_ext(safe_filename))
        mime_type <- switch(file_ext,
                            "pdf" = "application/pdf", "csv" = "text/csv", "doc" = "application/msword",
                            "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                            "xls" = "application/vnd.ms-excel", "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                            "application/octet-stream"
        )
        
        storage_endpoint <- paste0(base_url, "/storage/v1/object/swap-resources/", safe_filename)
        upload_res <- httr::POST(
          url = storage_endpoint,
          httr::add_headers(apikey = api_key, Authorization = paste("Bearer", user_token), `Content-Type` = mime_type),
          body = httr::upload_file(input$res_file$datapath, type = mime_type)
        )
        
        if (httr::status_code(upload_res) >= 400) {
          showNotification("File upload failed. Progress log aborted.", type = "error")
          return()
        }
        public_url <- paste0(base_url, "/storage/v1/object/public/swap-resources/", safe_filename)
        
        # Insert Resource into database
        is_species <- startsWith(input$res_target, "sp_")
        target_id <- as.integer(gsub("sp_|hab_", "", input$res_target))
        
        # We need a flag to stop execution if the DB insert fails
        resource_success <- TRUE 
        
        tryCatch({
          if(is_species) {
            # THE FIX: Changed uploaded_by to createdby
            dbExecute(db, "INSERT INTO track.speciesresources (speciesid, resource_name, resource_type, resource_url, createdby) VALUES ($1, $2, $3, $4, $5)",
                      params = list(target_id, input$res_name, input$res_type, public_url, current_user()$user_id))
          } else {
            # THE FIX: Changed uploaded_by to createdby
            dbExecute(db, "INSERT INTO track.habitatresources (habitatsubtypeid, resource_name, resource_type, resource_url, createdby) VALUES ($1, $2, $3, $4, $5)",
                      params = list(target_id, input$res_name, input$res_type, public_url, current_user()$user_id))
          }
        }, error = function(e) {
          # THE FIX: Show the exact SQL error so we know what's wrong!
          showNotification(paste("Resource DB Error:", e$message), type = "error", duration = 10)
          resource_success <<- FALSE
        })
        
        # If the resource failed to insert, abort the rest of the progress log
        if (!resource_success) return() 
      }
      
      # 2. Database Transaction for the Progress Log & Status Update
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          # Insert Historical Snapshot
          q_insert <- "INSERT INTO track.actiontracking (implementedactionid, actiondate, implementation_progress, result_progress, what_done, what_learned, what_needed, createdby) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
          dbExecute(conn, q_insert, params = list(as.integer(selected_row$implementedactionid), as.character(input$action_date), input$upd_impl_prog, input$upd_res_prog, input$what_done, input$what_learned, input$what_needed, current_user()$user_id))
          
          # Update Live Action Record
          q_update <- "UPDATE track.implementedactions SET implementation_progress = $1, result_progress = $2 WHERE implementedactionid = $3"
          dbExecute(conn, q_update, params = list(input$upd_impl_prog, input$upd_res_prog, as.integer(selected_row$implementedactionid)))
        })
        
        showNotification("Update/Progress successfully recorded!", type = "message", duration = 5)
        
        # Reset UI
        updateDateInput(session, "action_date", value = Sys.Date())
        updateTextAreaInput(session, "what_done", value = "")
        updateTextAreaInput(session, "what_learned", value = "")
        updateTextAreaInput(session, "what_needed", value = "")
        updateCheckboxInput(session, "add_resource_check", value = FALSE)
        updateTextInput(session, "res_name", value = "")
        
        db_sync_trigger(db_sync_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    }
    
    observeEvent(input$submit_progress, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      hist_df <- action_history()
      
      # Date Warning Check
      if (nrow(hist_df) > 0) {
        last_date <- as.Date(hist_df$Date[1]) 
        new_date <- as.Date(input$action_date)
        if (new_date <= last_date) {
          showModal(modalDialog(
            title = "Please Confirm Your Update",
            HTML(paste0("<p>We noticed a potential discrepancy:</p><ul><li>You are logging an update on <b>", new_date, "</b>, which is the same as or prior to the most recent logged date (<b>", last_date, "</b>).</li></ul><p>Are you sure you want to proceed?</p>")),
            footer = tagList(
              tagAppendAttributes(modalButton("Cancel"), style = "color: #333; background-color: #e9ecef; border-color: #ccc;"),
              actionButton(session$ns("confirm_progress_warning"), "Confirm & Save", class = "btn-warning", style="font-weight: bold;")
            )
          ))
          return()
        }
      }
      
      execute_progress_log(selected_row)
    })
    
    observeEvent(input$confirm_progress_warning, {
      removeModal()
      req(input$action_table_rows_selected)
      execute_progress_log(action_data()[input$action_table_rows_selected, ])
    })
    
  })
}