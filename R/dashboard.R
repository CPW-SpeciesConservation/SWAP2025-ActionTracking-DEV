dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # THE FIX: Force inactive pill tabs to be CPW Blue (or beige background) instead of white!
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
    
    h3("Statewide Action Tracking Dashboard", class = "mb-4", style = "color: #07234C !important; font-weight: bold;"),
    
    
    navset_card_pill(
      id = ns("dash_tabs"),
      
      # ==========================================
      # TAB 1: EXPLORE BY TARGET
      # ==========================================
      nav_panel("Explore by Target", 
                
                # TOP ROW: Selection and Actions Table
                fluidRow(
                  # Left Column: Selection
                  column(width = 4,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header text-white", style = "background-color: #055A53;", "1. Select Target"),
                             div(class = "card-body",
                                 p(em("Showing only targets with recorded actions.", style="font-size: 0.9em; color: #6c757d;")),
                                 radioButtons(ns("targ_type"), "Target Type:", choices = c("Species", "Habitat"), inline = TRUE),
                                 
                                 conditionalPanel(
                                   condition = sprintf("input['%s'] == 'Species'", ns("targ_type")),
                                   selectInput(ns("dash_tax_group"), "Taxonomic Group", choices = c("Loading..." = "")),
                                   selectizeInput(ns("dash_species"), "Species", choices = c("Select a group first..." = ""))
                                 ),
                                 conditionalPanel(
                                   condition = sprintf("input['%s'] == 'Habitat'", ns("targ_type")),
                                   selectInput(ns("dash_major_hab"), "Major Habitat", choices = c("Loading..." = "")),
                                   selectizeInput(ns("dash_habitat"), "Habitat Subtype", choices = c("Select a major habitat first..." = ""))
                                 )
                             )
                         )
                  ),
                  
                  # Right Column: Associated Actions Table
                  column(width = 8,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header bg-primary text-white", "2. Associated Actions"),
                             div(class = "card-body",
                                 p(em("Click a row to view progress updates and mitigated threats.", style = "color: #6c757d;")),
                                 DTOutput(ns("target_actions_table"))
                             )
                         )
                  )
                ),
                
                # BOTTOM ROW: Action Details (Strictly forced below the top row)
                conditionalPanel(
                  condition = sprintf("input['%s'] != null", ns("target_actions_table_rows_selected")),
                  fluidRow(
                    column(width = 12, h5("3. Action Progress & Details", class = "mb-3 mt-2", style="color: #07234C; border-bottom: 2px solid #ECE8E4; padding-bottom: 5px;"))
                  ),
                  fluidRow(
                    column(width = 5,
                           div(class = "card shadow-sm",
                               div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"),
                               div(class = "card-body", uiOutput(ns("targ_threats_ui")))
                           )
                    ),
                    column(width = 7,
                           div(class = "card shadow-sm",
                               div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Progress Updates"),
                               div(class = "card-body", DTOutput(ns("targ_updates_table")))
                           )
                    )
                  )
                )
      ),
      
      # ==========================================
      # TAB 2: EXPLORE BY ACTION
      # ==========================================
      nav_panel("Explore by Action",
                
                # TOP ROW: Selection and Targets Table
                fluidRow(
                  # Left Column: Selection
                  column(width = 4,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header text-white", style = "background-color: #055A53;", "1. Select Action"),
                             div(class = "card-body",
                                 p(em("Showing only actions currently active in the database.", style="font-size: 0.9em; color: #6c757d;")),
                                 selectInput(ns("dash_l0"), "Level 0 Category", choices = c("Loading..." = "")),
                                 selectInput(ns("dash_l1"), "Level 1 Category", choices = c("Select L0 first..." = "")),
                                 selectInput(ns("dash_l2"), "Level 2 Action", choices = c("Select L1 first..." = ""))
                             )
                         )
                  ),
                  
                  # Right Column: Targeted Species/Habitats Table
                  column(width = 8,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header bg-primary text-white", "2. Targets Receiving this Action"),
                             div(class = "card-body",
                                 p(em("Click a row to view progress updates and mitigated threats.", style = "color: #6c757d;")),
                                 DTOutput(ns("action_targets_table"))
                             )
                         )
                  )
                ),
                
                # BOTTOM ROW: Action Details
                conditionalPanel(
                  condition = sprintf("input['%s'] != null", ns("action_targets_table_rows_selected")),
                  fluidRow(
                    column(width = 12, h5("3. Implementation Progress & Details", class = "mb-3 mt-2", style="color: #07234C; border-bottom: 2px solid #ECE8E4; padding-bottom: 5px;"))
                  ),
                  fluidRow(
                    column(width = 5,
                           div(class = "card shadow-sm",
                               div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"),
                               div(class = "card-body", uiOutput(ns("act_threats_ui")))
                           )
                    ),
                    column(width = 7,
                           div(class = "card shadow-sm",
                               div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Progress Updates"),
                               div(class = "card-body", DTOutput(ns("act_updates_table")))
                           )
                    )
                  )
                )
      )
    )
  )
}

dashboard_server <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # INITIALIZE DROPDOWNS (STRICTLY FILTERED)
    # ==========================================
    observe({
      # Only Tax Groups that have mapped actions
      q_tax <- "SELECT DISTINCT tg.groupname FROM proj.taxonomicgroups tg JOIN proj.species s ON tg.taxonomicgroupid = s.taxonomicgroupid JOIN track.specieshabitatactions sha ON s.speciesid = sha.speciesid ORDER BY tg.groupname"
      tax_groups <- dbGetQuery(db, q_tax)
      ch_tax <- if(nrow(tax_groups) > 0) c("Choose a group..." = "", tax_groups$groupname) else c("No active data" = "")
      updateSelectInput(session, "dash_tax_group", choices = ch_tax)
      
      # Only Major Habitats that have mapped actions
      q_hab <- "SELECT DISTINCT mh.majorhabitatname FROM proj.majorhabitats mh JOIN proj.habitatsubtypes hs ON mh.majorhabitatid = hs.majorhabitatid JOIN track.specieshabitatactions sha ON hs.habitatsubtypeid = sha.habitatsubtypeid ORDER BY mh.majorhabitatname"
      habitats <- dbGetQuery(db, q_hab)
      ch_hab <- if(nrow(habitats) > 0) c("Choose a major habitat..." = "", habitats$majorhabitatname) else c("No active data" = "")
      updateSelectInput(session, "dash_major_hab", choices = ch_hab)
      
      # Only L0 Actions that are currently in the implementedactions table
      q_l0 <- "SELECT DISTINCT l0.actionl0id, l0.actionl0name FROM proj.l0_actions l0 JOIN proj.l1_actions l1 ON l0.actionl0id = l1.actionl0id JOIN proj.l2_actions l2 ON l1.actionl1id = l2.actionl1id JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id ORDER BY l0.actionl0name"
      l0_acts <- dbGetQuery(db, q_l0)
      ch_l0 <- if(nrow(l0_acts) > 0) c("Choose L0 Category..." = "", setNames(l0_acts$actionl0id, l0_acts$actionl0name)) else c("No active data" = "")
      updateSelectInput(session, "dash_l0", choices = ch_l0)
    })
    
    # ==========================================
    # TAB 1: EXPLORE BY TARGET LOGIC
    # ==========================================
    observeEvent(input$dash_tax_group, {
      req(input$dash_tax_group) 
      # Only Species that have mapped actions
      query <- "SELECT DISTINCT s.speciesid, s.commonname FROM proj.species s JOIN track.specieshabitatactions sha ON s.speciesid = sha.speciesid JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE tg.groupname = $1 ORDER BY s.commonname"
      species_list <- dbGetQuery(db, query, params = list(input$dash_tax_group))
      ch_sp <- if(nrow(species_list) > 0) c("Choose a species..." = "", setNames(species_list$speciesid, species_list$commonname)) else c("No active data" = "")
      updateSelectizeInput(session, "dash_species", choices = ch_sp)
    })
    
    observeEvent(input$dash_major_hab, {
      req(input$dash_major_hab) 
      # Only Subtypes that have mapped actions
      query <- "SELECT DISTINCT hs.habitatsubtypeid, hs.habitatsubtypename FROM proj.habitatsubtypes hs JOIN track.specieshabitatactions sha ON hs.habitatsubtypeid = sha.habitatsubtypeid JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE mh.majorhabitatname = $1 ORDER BY hs.habitatsubtypename"
      subtype_list <- dbGetQuery(db, query, params = list(input$dash_major_hab))
      ch_sub <- if(nrow(subtype_list) > 0) c("Choose a subtype..." = "", setNames(subtype_list$habitatsubtypeid, subtype_list$habitatsubtypename)) else c("No active data" = "")
      updateSelectizeInput(session, "dash_habitat", choices = ch_sub)
    })
    
    targ_actions_data <- reactive({
      is_species <- input$targ_type == "Species"
      target_id <- if(is_species) input$dash_species else input$dash_habitat
      if (is.null(target_id) || target_id == "") return(data.frame())
      
      query <- if(is_species) {
        "SELECT ia.implementedactionid, l2.actionl2name AS \"Action\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id WHERE sha.speciesid = $1"
      } else {
        "SELECT ia.implementedactionid, l2.actionl2name AS \"Action\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id WHERE sha.habitatsubtypeid = $1"
      }
      dbGetQuery(db, query, params = list(as.integer(target_id)))
    })
    
    output$target_actions_table <- renderDT({
      df <- targ_actions_data()
      if(nrow(df) == 0) return(datatable(data.frame(Message = "Please select a target on the left."), rownames = FALSE, options = list(dom = 't')))
      datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 5, dom = 'tp', columnDefs = list(list(visible = FALSE, targets = 0))))
    })
    
    output$targ_threats_ui <- renderUI({
      req(input$target_actions_table_rows_selected)
      impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.implementedactionid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      
      if(nrow(threats) == 0) return(p("No threats recorded."))
      tags$ul(class = "ps-3", lapply(1:nrow(threats), function(i) tags$li(strong(threats$t_name[i]), br(), em(threats$justification[i]), class="mb-3")))
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
      # Only L1 Actions that are currently in the implementedactions table
      q <- "SELECT DISTINCT l1.actionl1id, l1.actionl1code || '. ' || l1.actionl1name AS n FROM proj.l1_actions l1 JOIN proj.l2_actions l2 ON l1.actionl1id = l2.actionl1id JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l1.actionl0id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l0))
      ch_l1 <- if(nrow(res) > 0) c("Select L1 Category..." = "", setNames(res$actionl1id, res$n)) else c("No active data" = "")
      updateSelectInput(session, "dash_l1", choices = ch_l1)
    })
    
    observeEvent(input$dash_l1, {
      req(input$dash_l1)
      # Only L2 Actions that are currently in the implementedactions table
      q <- "SELECT DISTINCT l2.actionl2id, l2.actionl2code || '. ' || l2.actionl2name AS n FROM proj.l2_actions l2 JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l2.actionl1id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l1))
      ch_l2 <- if(nrow(res) > 0) c("Select L2 Action..." = "", setNames(res$actionl2id, res$n)) else c("No active data" = "")
      updateSelectInput(session, "dash_l2", choices = ch_l2)
    })
    
    act_targets_data <- reactive({
      req(input$dash_l2)
      query <- "
        SELECT 
          ia.implementedactionid,
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
      datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 5, dom = 'tp', columnDefs = list(list(visible = FALSE, targets = 0))))
    })
    
    output$act_threats_ui <- renderUI({
      req(input$action_targets_table_rows_selected)
      impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.implementedactionid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      
      if(nrow(threats) == 0) return(p("No threats recorded."))
      tags$ul(class = "ps-3", lapply(1:nrow(threats), function(i) tags$li(strong(threats$t_name[i]), br(), em(threats$justification[i]), class="mb-3")))
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