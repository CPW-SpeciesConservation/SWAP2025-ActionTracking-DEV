update_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Update an Existing Action", class = "mb-4"),
    p("Select an action below to view its full details and log new progress."),
    
    # 1. YOUR ACTIONS TABLE
    div(class = "card mb-4 shadow-sm",
        div(class = "card-header bg-primary text-white", "Your Actions & Delegations"),
        div(class = "card-body",
            DTOutput(ns("action_table"))
        )
    ),
    
    # This panel only shows up when a row in the table is clicked
    conditionalPanel(
      condition = sprintf("input['%s'] != null", ns("action_table_rows_selected")),
      
      # 2. FULL ACTION DETAILS (NEW)
      div(class = "card mb-4 shadow-sm", style = "border-color: #07234C;",
          div(class = "card-header text-white", style = "background-color: #07234C;", "Original Action Details"),
          div(class = "card-body bg-light",
              uiOutput(ns("full_action_details"))
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
                numericInput(ns("stat_value"), "Value", value = 0, min = 0, width = "100%")
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

update_action_server <- function(id, db, current_user) {
  moduleServer(id, function(input, output, session) {
    
    refresh_trigger <- reactiveVal(0)
    
    # --- 1. FETCH REAL ACTIONS ---
    action_data <- reactive({
      refresh_trigger()
      query <- "
        SELECT DISTINCT
          ia.implementedactionid,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS \"Target\",
          l2.actionl2name AS \"Action\",
          ia.timeframe AS \"Timeframe\",
          ia.status AS \"Status\",
          CASE WHEN ia.createdby = $1 THEN 'Creator' ELSE 'Delegate' END AS \"Role\"
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1 OR du.userid = $2
      "
      dbGetQuery(db, query, params = list(current_user()$FirstName, current_user()$UserID))
    })
    
    output$action_table <- renderDT({
      datatable(action_data(), selection = "single", rownames = FALSE, 
                options = list(pageLength = 5, dom = 'ftp', columnDefs = list(list(visible = FALSE, targets = 0))))
    })
    
    # --- 2. FETCH AND RENDER FULL ACTION DETAILS (NEW) ---
    output$full_action_details <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      # Query to get the Description and the specific Action Detail
      q_info <- "
        SELECT 
          ia.actiondesc,
          sadd.\"Meaningful.Details\" AS species_detail,
          hadd.\"Meaningful.Details\" AS habitat_detail
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid
        LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid
        WHERE ia.implementedactionid = $1
        LIMIT 1
      "
      info_df <- dbGetQuery(db, q_info, params = list(as.integer(impl_id)))
      
      # Query to get the Threats and Justifications
      q_threats <- "
        SELECT 
          l2.threatl2code || '. ' || l2.threatl2name AS threat_name,
          ta.justification
        FROM track.threatsaddressed ta
        JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid
        JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id
        WHERE sha.implementedactionid = $1
      "
      threats_df <- dbGetQuery(db, q_threats, params = list(as.integer(impl_id)))
      
      # Format the data safely (handling NULLs)
      desc_text <- if(is.na(info_df$actiondesc[1]) || info_df$actiondesc[1] == "") "No description provided." else info_df$actiondesc[1]
      
      detail_text <- "None Selected"
      if (!is.na(info_df$species_detail[1])) detail_text <- info_df$species_detail[1]
      if (!is.na(info_df$habitat_detail[1])) detail_text <- info_df$habitat_detail[1]
      
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      # Build the Threat List HTML
      threat_ui <- if(nrow(threats_df) > 0) {
        tags$ul(class = "mt-2",
                lapply(1:nrow(threats_df), function(i) {
                  tags$li(
                    strong(threats_df$threat_name[i]), br(),
                    em("Justification: "), threats_df$justification[i]
                  )
                })
        )
      } else {
        p(em("No threats recorded."))
      }
      
      # Return the structured UI block
      tagList(
        layout_columns(
          div(
            h6("Core Info", class = "text-muted mb-1"),
            p(strong("Target: "), selected_row$Target, br(),
              strong("Action: "), selected_row$Action, br(),
              strong("Timeframe: "), selected_row$Timeframe, br(),
              strong("Status: "), selected_row$Status)
          ),
          div(
            h6("Action Specifics", class = "text-muted mb-1"),
            p(strong("Lexicon Detail: "), detail_text, br(),
              strong("User Description: "), desc_text)
          )
        ),
        hr(),
        h6("Threats Addressed & Justifications", class = "text-muted mb-1"),
        threat_ui
      )
    })
    
    # --- 3. FETCH AND RENDER PREVIOUS UPDATES ---
    action_history <- reactive({
      req(input$action_table_rows_selected)
      refresh_trigger() 
      
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      query <- "
        SELECT actiondate::date AS \"Date\", stattype AS \"Metric\", stat AS \"Value\", comments AS \"Notes\", createdby AS \"Entered By\"
        FROM track.actiontracking
        WHERE implementedactionid = $1
        ORDER BY actiondate DESC
      "
      dbGetQuery(db, query, params = list(as.integer(impl_id)))
    })
    
    output$history_table <- renderDT({
      df <- action_history()
      if(nrow(df) == 0) {
        datatable(df, rownames = FALSE, options = list(dom = 't', language = list(emptyTable = "No updates have been recorded for this action yet.")))
      } else {
        datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = 'tp'))
      }
    })
    
    # --- 4. SUBMIT NEW UPDATE ---
    observeEvent(input$submit_update, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      
      tryCatch({
        q_insert <- "INSERT INTO track.actiontracking (implementedactionid, actiondate, stattype, stat, comments, createdby) VALUES ($1, $2, $3, $4, $5, $6)"
        dbExecute(db, q_insert, params = list(as.integer(selected_row$implementedactionid), as.character(input$action_date), input$stat_type, as.numeric(input$stat_value), input$comments, current_user()$FirstName))
        
        showNotification("Update successfully recorded!", type = "message", duration = 5)
        updateNumericInput(session, "stat_value", value = 0)
        updateTextAreaInput(session, "comments", value = "")
        refresh_trigger(refresh_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    })
  })
}