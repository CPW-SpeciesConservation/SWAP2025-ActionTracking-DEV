library(memoise)
library(cachem)

# --- GLOBAL SHARED CACHE ---
global_db_cache <- cachem::cache_mem(max_age = 3600)

get_dash_tax_groups <- memoise(function(db) {
  dbGetQuery(db, "SELECT DISTINCT tg.groupname FROM proj.taxonomicgroups tg JOIN proj.species s ON tg.taxonomicgroupid = s.taxonomicgroupid JOIN track.specieshabitatactions sha ON s.speciesid = sha.speciesid ORDER BY tg.groupname")
}, cache = global_db_cache)

get_dash_major_habitats <- memoise(function(db) {
  dbGetQuery(db, "SELECT DISTINCT mh.majorhabitatname FROM proj.majorhabitats mh JOIN proj.habitatsubtypes hs ON mh.majorhabitatid = hs.majorhabitatid JOIN track.specieshabitatactions sha ON hs.habitatsubtypeid = sha.habitatsubtypeid ORDER BY mh.majorhabitatname")
}, cache = global_db_cache)

get_dash_l0_actions <- memoise(function(db) {
  dbGetQuery(db, "SELECT DISTINCT l0.actionl0id, l0.actionl0name FROM proj.l0_actions l0 JOIN proj.l1_actions l1 ON l0.actionl0id = l1.actionl0id JOIN proj.l2_actions l2 ON l1.actionl1id = l2.actionl1id JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id ORDER BY l0.actionl0name")
}, cache = global_db_cache)
# --------------------------------

dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$head(
      tags$style(HTML("
        .nav-pills .nav-link { 
          color: #07234C !important; 
          background-color: #ECE8E4; 
          font-weight: bold; 
          margin-right: 5px; 
          border-radius: 5px;
        }
        .nav-pills .nav-link.active { 
          color: #FFFFFF !important; 
          background-color: #0D67B8 !important; 
        }
      "))
    ),
    
    h3("SWAP Action Tracking Dashboard", class = "mb-4", style = "color: #07234C !important; font-weight: bold;"),
    
    navset_underline(
      id = ns("dash_tabs"),
      
      # ==========================================
      # TAB 1: EXPLORE BY TARGET
      # ==========================================
      nav_panel("Explore by Species/Habitat", 
                
                fluidRow(
                  column(width = 4,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header text-white", style = "background-color: #055A53;", "Select Species/Habitat"),
                             div(class = "card-body",
                                 p(em("Showing only targets with recorded actions.", style="font-size: 0.9em; color: #6c757d;")),
                                 radioButtons(ns("targ_type"), "Type:", choices = c("Species", "Habitat"), inline = TRUE),
                                 
                                 conditionalPanel(
                                   condition = sprintf("input['%s'] == 'Species'", ns("targ_type")),
                                   selectizeInput(ns("dash_tax_group"), "Taxonomic Group", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                                   selectizeInput(ns("dash_species"), "Species", choices = c("Select a group first..." = ""),options = list(dropdownParent = "body"))
                                 ),
                                 conditionalPanel(
                                   condition = sprintf("input['%s'] == 'Habitat'", ns("targ_type")),
                                   selectizeInput(ns("dash_major_hab"), "Major Habitat", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                                   selectizeInput(ns("dash_habitat"), "Habitat Subtype", choices = c("Select a major habitat first..." = ""),options = list(dropdownParent = "body"))
                                 )
                             )
                         )
                  ),
                  
                  column(width = 8,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header bg-primary text-white", "Tracked Actions"),
                             div(class = "card-body",
                                 p(em("Click a row to view progress updates and mitigated threats.", style = "color: #6c757d;")),
                                 DTOutput(ns("target_actions_table"))
                             )
                         )
                  )
                ),
                
                condition = sprintf("input['%s'] != null", ns("target_actions_table_rows_selected")),
                fluidRow(
                  column(width = 12, h5("Action Updates & Details", class = "mb-3 mt-2", style="color: #07234C; border-bottom: 2px solid #ECE8E4; padding-bottom: 5px;"))
                ),
                fluidRow( column(width = 7,
                                 div(class = "card shadow-sm",
                                     div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Progress Updates"),
                                     div(class = "card-body", DTOutput(ns("targ_updates_table")))
                                 )
                ),
                column(width = 5,
                       div(class = "card shadow-sm mb-3",
                           div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"),
                           div(class = "card-body", uiOutput(ns("targ_threats_ui")))
                       ),
                       # NEW: Co-targets card
                       div(class = "card shadow-sm",
                           div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Other Targets in this Action"),
                           div(class = "card-body", uiOutput(ns("targ_other_targets_ui")))
                       )
                )
                )
      ),
      
      # ==========================================
      # TAB 2: EXPLORE BY ACTION
      # ==========================================
      nav_panel("Explore by Action",
                
                fluidRow(
                  column(width = 4,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header text-white", style = "background-color: #055A53;", "Select Action"),
                             div(class = "card-body",
                                 p(em("Showing only actions currently active in the database.", style="font-size: 0.9em; color: #6c757d;")),
                                 selectizeInput(ns("dash_l0"), "Level 0 Category", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                                 selectizeInput(ns("dash_l1"), "Level 1 Category", choices = c("Select Level 0 first..." = ""),options = list(dropdownParent = "body")),
                                 selectizeInput(ns("dash_l2"), "Level 2 Action", choices = c("Select Level 1 first..." = ""),options = list(dropdownParent = "body"))
                             )
                         )
                  ),
                  
                  column(width = 8,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header bg-primary text-white", "Species/Habitats targeted by this Action"),
                             div(class = "card-body",
                                 p(em("Click a row to view progress updates and mitigated threats.", style = "color: #6c757d;")),
                                 DTOutput(ns("action_targets_table"))
                             )
                         )
                  )
                ),
                
                condition = sprintf("input['%s'] != null", ns("action_targets_table_rows_selected")),
                fluidRow(
                  column(width = 12, h5("Action Updates & Details", class = "mb-3 mt-2", style="color: #07234C; border-bottom: 2px solid #ECE8E4; padding-bottom: 5px;"))
                ),
                fluidRow( column(width = 7,
                                 div(class = "card shadow-sm",
                                     div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Progress Updates"),
                                     div(class = "card-body", DTOutput(ns("act_updates_table")))
                                 )
                ),
                column(width = 5,
                       div(class = "card shadow-sm mb-3",
                           div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"),
                           div(class = "card-body", uiOutput(ns("act_threats_ui")))
                       ),
                       # NEW: Co-targets card for Tab 2
                       div(class = "card shadow-sm",
                           div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Other Targets in this Action"),
                           div(class = "card-body", uiOutput(ns("act_other_targets_ui")))
                       )
                )
                )
      )
    )
  )
}

dashboard_server <- function(id, db, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # INITIALIZE DROPDOWNS (CACHED & FILTERED)
    # ==========================================
    observe({
      db_sync_trigger() # Listen for global updates
      
      tax_groups <- get_dash_tax_groups(db)
      ch_tax <- if(nrow(tax_groups) > 0) c("Select a group..." = "", tax_groups$groupname) else c("No active data" = "")
      updateSelectInput(session, "dash_tax_group", choices = ch_tax)
      
      habitats <- get_dash_major_habitats(db)
      ch_hab <- if(nrow(habitats) > 0) c("Select a major habitat..." = "", habitats$majorhabitatname) else c("No active data" = "")
      updateSelectInput(session, "dash_major_hab", choices = ch_hab)
      
      l0_acts <- get_dash_l0_actions(db)
      ch_l0 <- if(nrow(l0_acts) > 0) c("Select a Level 0 Category..." = "", setNames(l0_acts$actionl0id, l0_acts$actionl0name)) else c("No active data" = "")
      updateSelectInput(session, "dash_l0", choices = ch_l0)
    })
    
    # ==========================================
    # TAB 1: EXPLORE BY TARGET LOGIC
    # ==========================================
    observeEvent(input$dash_tax_group, {
      req(input$dash_tax_group) 
      query <- "SELECT DISTINCT s.speciesid, s.commonname FROM proj.species s JOIN track.specieshabitatactions sha ON s.speciesid = sha.speciesid JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE tg.groupname = $1 ORDER BY s.commonname"
      species_list <- dbGetQuery(db, query, params = list(input$dash_tax_group))
      ch_sp <- if(nrow(species_list) > 0) c("Choose a species..." = "", setNames(species_list$speciesid, species_list$commonname)) else c("No active data" = "")
      updateSelectizeInput(session, "dash_species", choices = ch_sp)
    })
    
    observeEvent(input$dash_major_hab, {
      req(input$dash_major_hab) 
      query <- "SELECT DISTINCT hs.habitatsubtypeid, hs.habitatsubtypename FROM proj.habitatsubtypes hs JOIN track.specieshabitatactions sha ON hs.habitatsubtypeid = sha.habitatsubtypeid JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE mh.majorhabitatname = $1 ORDER BY hs.habitatsubtypename"
      subtype_list <- dbGetQuery(db, query, params = list(input$dash_major_hab))
      ch_sub <- if(nrow(subtype_list) > 0) c("Choose a habitat subtype..." = "", setNames(subtype_list$habitatsubtypeid, subtype_list$habitatsubtypename)) else c("No active data" = "")
      updateSelectizeInput(session, "dash_habitat", choices = ch_sub)
    })
    
    targ_actions_data <- reactive({
      db_sync_trigger()
      is_species <- input$targ_type == "Species"
      target_id <- if(is_species) input$dash_species else input$dash_habitat
      if (is.null(target_id) || target_id == "") return(data.frame())
      
      # THE FIX: Added sha.specieshabitatactionsid to the SELECT
      query <- if(is_species) {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2name AS \"Action\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id WHERE sha.speciesid = $1"
      } else {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2name AS \"Action\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id WHERE sha.habitatsubtypeid = $1"
      }
      dbGetQuery(db, query, params = list(as.integer(target_id)))
    })
    
    output$target_actions_table <- renderDT({
      df <- targ_actions_data()
      if(nrow(df) == 0) return(datatable(data.frame(Message = "Please select a target on the left."), rownames = FALSE, options = list(dom = 't')))
      # THE FIX: Hide columns 0 AND 1 (both internal IDs)
      datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 5, dom = 'tp', columnDefs = list(list(visible = FALSE, targets = c(0, 1)))))
    })
    
    output$targ_threats_ui <- renderUI({
      req(input$target_actions_table_rows_selected)
      # THE FIX: Query by specieshabitatactionsid, not the overarching implementedactionid
      sha_id <- targ_actions_data()[input$target_actions_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.specieshabitatactionsid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(sha_id)))
      
      if(nrow(threats) == 0) return(p("No threats recorded for this specific target."))
      tags$ul(class = "ps-3", lapply(1:nrow(threats), function(i) tags$li(strong(threats$t_name[i]), br(), em(threats$justification[i]), class="mb-3")))
    })
    
    # NEW: Query for co-targets
    output$targ_other_targets_ui <- renderUI({
      req(input$target_actions_table_rows_selected)
      impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]
      sha_id <- targ_actions_data()[input$target_actions_table_rows_selected, "specieshabitatactionsid"]
      
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 AND sha.specieshabitatactionsid != $2"
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id), as.integer(sha_id)))
      
      if(nrow(others) == 0) return(p("This action applies exclusively to this target."))
      tags$ul(class = "ps-3", lapply(1:nrow(others), function(i) tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")")))
    })
    
    output$targ_updates_table <- renderDT({
      req(input$target_actions_table_rows_selected)
      impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT actiondate::date AS \"Date\", stattype AS \"Metric\", stat AS \"Value\", comments AS \"Notes\" FROM track.actiontracking WHERE implementedactionid = $1 ORDER BY actiondate DESC"
      df <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(df) == 0) return(datatable(data.frame(Message="No progress updates have been recorded yet."), rownames=F, options=list(dom='t')))
      datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = 'tp'))
    })
    
    # ==========================================
    # TAB 2: EXPLORE BY ACTION LOGIC
    # ==========================================
    observeEvent(input$dash_l0, {
      req(input$dash_l0)
      q <- "SELECT DISTINCT l1.actionl1id, l1.actionl1code || '. ' || l1.actionl1name AS n FROM proj.l1_actions l1 JOIN proj.l2_actions l2 ON l1.actionl1id = l2.actionl1id JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l1.actionl0id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l0))
      ch_l1 <- if(nrow(res) > 0) c("Select a Level 1 Action..." = "", setNames(res$actionl1id, res$n)) else c("No active data" = "")
      updateSelectInput(session, "dash_l1", choices = ch_l1)
    })
    
    observeEvent(input$dash_l1, {
      req(input$dash_l1)
      q <- "SELECT DISTINCT l2.actionl2id, l2.actionl2code || '. ' || l2.actionl2name AS n FROM proj.l2_actions l2 JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l2.actionl1id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l1))
      ch_l2 <- if(nrow(res) > 0) c("Select a Level 2 Action..." = "", setNames(res$actionl2id, res$n)) else c("No active data" = "")
      updateSelectInput(session, "dash_l2", choices = ch_l2)
    })
    
    act_targets_data <- reactive({
      db_sync_trigger()
      if (is.null(input$dash_l2) || input$dash_l2 == "") return(data.frame())
      
      # THE FIX: Added sha.specieshabitatactionsid to the SELECT
      query <- "
        SELECT 
          ia.implementedactionid,
          sha.specieshabitatactionsid,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS \"Target Name\",
          CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS \"Type\",
          ia.status AS \"Status\",
          ia.timeframe AS \"Timeframe\"
        FROM track.implementedactions ia
        JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        WHERE ia.actionl2id = $1
      "
      dbGetQuery(db, query, params = list(as.integer(input$dash_l2)))
    })
    
    output$action_targets_table <- renderDT({
      df <- act_targets_data()
      if(nrow(df) == 0) return(datatable(data.frame(Message = "Please select an action on the left."), rownames = FALSE, options = list(dom = 't')))
      # THE FIX: Hide columns 0 AND 1 (both internal IDs)
      datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 5, dom = 'tp', columnDefs = list(list(visible = FALSE, targets = c(0, 1)))))
    })
    
    output$act_threats_ui <- renderUI({
      req(input$action_targets_table_rows_selected)
      # THE FIX: Query by specieshabitatactionsid, not implementedactionid
      sha_id <- act_targets_data()[input$action_targets_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.specieshabitatactionsid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(sha_id)))
      
      if(nrow(threats) == 0) return(p("No threats recorded for this specific target."))
      tags$ul(class = "ps-3", lapply(1:nrow(threats), function(i) tags$li(strong(threats$t_name[i]), br(), em(threats$justification[i]), class="mb-3")))
    })
    
    # NEW: Query for co-targets
    output$act_other_targets_ui <- renderUI({
      req(input$action_targets_table_rows_selected)
      impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      sha_id <- act_targets_data()[input$action_targets_table_rows_selected, "specieshabitatactionsid"]
      
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 AND sha.specieshabitatactionsid != $2"
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id), as.integer(sha_id)))
      
      if(nrow(others) == 0) return(p("This action applies exclusively to this target."))
      tags$ul(class = "ps-3", lapply(1:nrow(others), function(i) tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")")))
    })
    
    output$act_updates_table <- renderDT({
      req(input$action_targets_table_rows_selected)
      impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT actiondate::date AS \"Date\", stattype AS \"Metric\", stat AS \"Value\", comments AS \"Notes\" FROM track.actiontracking WHERE implementedactionid = $1 ORDER BY actiondate DESC"
      df <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(df) == 0) return(datatable(data.frame(Message="No progress updates have been recorded yet."), rownames=F, options=list(dom='t')))
      datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = 'tp'))
    })
    
  })
}