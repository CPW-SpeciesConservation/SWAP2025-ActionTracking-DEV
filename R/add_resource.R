library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(DBI)
library(shinyjs) # Added for the reset functionality

add_resource_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # THE FIX 1: CSS to darken the Browse button
    tags$style(HTML("
      .btn-file { background-color: #0D67B8 !important; color: #FFFFFF !important; font-weight: bold; border: none; }
      .btn-file:hover { background-color: #07234C !important; }
    ")),
    
    div(class = "container mt-4", style = "max-width: 800px;",
        div(class = "card shadow-sm",
            div(class = "card-header text-white fw-bold", style = "background-color: #07234C;", "Add a New Resource"),
            div(class = "card-body",
                p(em("Upload a document or link to an external website to build the Resource Library for a specific Species or Habitat.")),
                hr(),
                
                # 1. Target Selection
                h5("Select Species/Habitat", class = "mt-3 fw-bold"),
                radioButtons(ns("target_type"), "Is this resource for a Species or Habitat?", 
                             choices = c("Species", "Habitat"), inline = TRUE),
                
                fluidRow(
                  column(6, 
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'Species'", ns("target_type")),
                           selectizeInput(ns("tax_group"), "Taxonomic Group", choices = c("Loading..." = ""))
                         ),
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'Habitat'", ns("target_type")),
                           selectizeInput(ns("major_hab"), "Major Habitat", choices = c("Loading..." = ""))
                         )
                  ),
                  column(6, 
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'Species'", ns("target_type")),
                           selectizeInput(ns("species_id"), "Specific Species", choices = c("Select a group first..." = ""))
                         ),
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'Habitat'", ns("target_type")),
                           selectizeInput(ns("habitat_id"), "Habitat Subtype", choices = c("Select a habitat first..." = ""))
                         )
                  )
                ),
                
                hr(),
                
                # 2. Resource Details
                h5("Resource Details", class = "mt-3 fw-bold"),
                textInput(ns("resource_name"), "Resource Name / Title", width = "100%", placeholder = "e.g., 2023 Colorado River Cutthroat Trout Conservation Strategy"),
                
                radioButtons(ns("resource_type"), "Resource Format:", 
                             choices = c("URL (Link to website)", "Document (Upload File)"), inline = TRUE),
                
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'URL (Link to website)'", ns("resource_type")),
                  textInput(ns("resource_url"), "Website URL", width = "100%", placeholder = "https://...")
                ),
                
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'Document (Upload File)'", ns("resource_type")),
                  fileInput(ns("resource_file"), "Upload Document (PDF, Word, Excel)", width = "100%",
                            accept = c(".pdf", ".doc", ".docx", ".xls", ".xlsx", ".csv"))
                ),
                
                hr(),
                
                # Submit & Messages
                actionButton(ns("btn_submit"), "Add Resource to Library", class = "btn-success w-100", style = "font-weight: bold; font-size: 1.1rem;"),
                div(class = "mt-3 text-center", uiOutput(ns("status_msg")))
            )
        )
    )
  )
}

add_resource_server <- function(id, db, current_user) {
  moduleServer(id, function(input, output, session) {
    
    # --- Populate Initial Dropdowns ---
    observe({
      tax_groups <- dbGetQuery(db, "SELECT DISTINCT groupname FROM proj.taxonomicgroups ORDER BY groupname")
      updateSelectizeInput(session, "tax_group", choices = c("Select group..." = "", tax_groups$groupname))
      
      habs <- dbGetQuery(db, "SELECT DISTINCT majorhabitatname FROM proj.majorhabitats ORDER BY majorhabitatname")
      updateSelectizeInput(session, "major_hab", choices = c("Select habitat..." = "", habs$majorhabitatname))
    })
    
    observeEvent(input$tax_group, {
      req(input$tax_group)
      q <- "SELECT s.speciesid, s.commonname FROM proj.species s JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE tg.groupname = $1 ORDER BY s.commonname"
      res <- dbGetQuery(db, q, params = list(input$tax_group))
      updateSelectizeInput(session, "species_id", choices = c("Choose species..." = "", setNames(res$speciesid, res$commonname)))
    })
    
    observeEvent(input$major_hab, {
      req(input$major_hab)
      q <- "SELECT hs.habitatsubtypeid, hs.habitatsubtypename FROM proj.habitatsubtypes hs JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE mh.majorhabitatname = $1 ORDER BY hs.habitatsubtypename"
      res <- dbGetQuery(db, q, params = list(input$major_hab))
      updateSelectizeInput(session, "habitat_id", choices = c("Choose subtype..." = "", setNames(res$habitatsubtypeid, res$habitatsubtypename)))
    })
    
    # --- Handle Form Submission ---
    observeEvent(input$btn_submit, {
      # 1. Validation
      is_species <- input$target_type == "Species"
      target_val <- if(is_species) input$species_id else input$habitat_id
      
      if (is.null(target_val) || target_val == "") {
        output$status_msg <- renderUI(span(style="color: red; font-weight: bold;", "Error: Please select a specific Species or Habitat."))
        return()
      }
      if (trimws(input$resource_name) == "") {
        output$status_msg <- renderUI(span(style="color: red; font-weight: bold;", "Error: Please provide a Resource Name."))
        return()
      }
      
      is_url <- input$resource_type == "URL (Link to website)"
      if (is_url && trimws(input$resource_url) == "") {
        output$status_msg <- renderUI(span(style="color: red; font-weight: bold;", "Error: Please provide a valid URL."))
        return()
      }
      if (!is_url && is.null(input$resource_file)) {
        output$status_msg <- renderUI(span(style="color: red; font-weight: bold;", "Error: Please upload a file."))
        return()
      }
      
      output$status_msg <- renderUI(span(style="color: #0D67B8; font-weight: bold;", "Processing and saving resource..."))
      
      # 2. Determine Final URL (Upload if necessary)
      final_url <- ""
      db_res_type <- if(is_url) "URL" else "Document"
      
      if (is_url) {
        final_url <- trimws(input$resource_url)
      } else {
        # Supabase Storage Upload Logic
        base_url <- Sys.getenv("SUPABASE_URL")
        api_key <- Sys.getenv("SUPABASE_ANON_KEY")
        user_token <- current_user()$token
        
        # Clean the filename to prevent URL issues, prepend timestamp to ensure uniqueness
        safe_filename <- paste0(as.integer(Sys.time()), "_", gsub("[^[:alnum:]._-]", "", input$resource_file$name))
        
        # THE FIX: Determine the correct MIME type based on the file extension
        file_ext <- tolower(tools::file_ext(safe_filename))
        mime_type <- switch(file_ext,
                            "pdf" = "application/pdf",
                            "csv" = "text/csv",
                            "doc" = "application/msword",
                            "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                            "xls" = "application/vnd.ms-excel",
                            "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                            "application/octet-stream" # Fallback
        )
        
        # Endpoint to upload to our 'swap-resources' bucket
        storage_endpoint <- paste0(base_url, "/storage/v1/object/swap-resources/", safe_filename)
        
        # Push file to Supabase via API
        upload_res <- httr::POST(
          url = storage_endpoint,
          httr::add_headers(
            apikey = api_key,
            Authorization = paste("Bearer", user_token),
            `Content-Type` = mime_type # <-- Pass the correct MIME type here!
          ),
          body = httr::upload_file(input$resource_file$datapath, type = mime_type)
        )
        
        if (httr::status_code(upload_res) >= 400) {
          err <- httr::content(upload_res)
          output$status_msg <- renderUI(span(style="color: red; font-weight: bold;", paste("File upload failed:", err$message)))
          return()
        }
        
        # Construct the permanent public URL 
        final_url <- paste0(base_url, "/storage/v1/object/public/swap-resources/", safe_filename)
      }
      
      # 3. Save to Database
      user_uuid <- current_user()$user_id
      
      tryCatch({
        if (is_species) {
          q_insert <- "INSERT INTO track.speciesresources (speciesid, resource_name, resource_type, resource_url, createdby) VALUES ($1, $2, $3, $4, $5)"
          dbExecute(db, q_insert, params = list(as.integer(target_val), trimws(input$resource_name), db_res_type, final_url, user_uuid))
        } else {
          q_insert <- "INSERT INTO track.habitatresources (habitatsubtypeid, resource_name, resource_type, resource_url, createdby) VALUES ($1, $2, $3, $4, $5)"
          dbExecute(db, q_insert, params = list(as.integer(target_val), trimws(input$resource_name), db_res_type, final_url, user_uuid))
        }
        
        # THE FIX 2: Reset UI Text AND clear the File Input cache using shinyjs
        updateTextInput(session, "resource_name", value = "")
        updateTextInput(session, "resource_url", value = "")
        shinyjs::reset(session$ns("resource_file")) # Empties the upload box!
        
        output$status_msg <- renderUI(span(style="color: green; font-weight: bold;", "Success! Resource added to the library."))
        
      }, error = function(e) {
        output$status_msg <- renderUI(span(style="color: red; font-weight: bold;", paste("Database Error:", e$message)))
      })
    })
  })
}