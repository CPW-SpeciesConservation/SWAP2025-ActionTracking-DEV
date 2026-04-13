library(memoise)
library(cachem)
library(ggplot2)
library(plotly)
library(igraph)
library(ggraph)
library(ggiraph) # NEW: For interactive edge bundling
library(stringr) # NEW: For wrapping long labels
library(dplyr)

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
                             div(class = "card-header text-white", style = "background-color: #055A53;", "Filter & Select Target"),
                             div(class = "card-body",
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
                                 ),
                                 hr(class = "my-4"),
                                 h6("Tracked Actions for Selected Target", class = "fw-bold text-muted"),
                                 DTOutput(ns("target_actions_table"))
                             )
                         )
                  ),
                  column(width = 8,
                         conditionalPanel(
                           condition = sprintf("input['%s'] == null", ns("target_actions_table_rows_selected")),
                           div(class = "card shadow-sm", style = "background-color: #FAFAFA; border: 2px dashed #CCCCCC;",
                               div(class = "card-body text-center text-muted", style = "padding: 120px 20px;",
                                   h4("Select an action on the left to view its progress.")
                               )
                           )
                         ),
                         conditionalPanel(
                           condition = sprintf("input['%s'] != null", ns("target_actions_table_rows_selected")),
                           fluidRow( 
                             column(width = 7,
                                    div(class = "card shadow-sm",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Progress Updates"),
                                        div(class = "card-body", 
                                            plotlyOutput(ns("targ_progress_plot"), height = "250px"),
                                            hr(),
                                            DTOutput(ns("targ_updates_table"))
                                        )
                                    )
                             ),
                             column(width = 5,
                                    div(class = "card shadow-sm mb-3",
                                        div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"),
                                        div(class = "card-body", uiOutput(ns("targ_threats_ui")))
                                    ),
                                    div(class = "card shadow-sm",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Other Targets in this Action"),
                                        div(class = "card-body", uiOutput(ns("targ_other_targets_ui")))
                                    )
                             )
                           )
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
                             div(class = "card-header text-white", style = "background-color: #055A53;", "Filter & Select Action"),
                             div(class = "card-body",
                                 selectizeInput(ns("dash_l0"), "Level 0 Category", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                                 selectizeInput(ns("dash_l1"), "Level 1 Category", choices = c("Select Level 0 first..." = ""),options = list(dropdownParent = "body")),
                                 selectizeInput(ns("dash_l2"), "Level 2 Action", choices = c("Select Level 1 first..." = ""),options = list(dropdownParent = "body")),
                                 hr(class = "my-4"),
                                 h6("Targets for Selected Action", class = "fw-bold text-muted"),
                                 DTOutput(ns("action_targets_table"))
                             )
                         )
                  ),
                  column(width = 8,
                         conditionalPanel(
                           condition = sprintf("input['%s'] == null", ns("action_targets_table_rows_selected")),
                           div(class = "card shadow-sm", style = "background-color: #FAFAFA; border: 2px dashed #CCCCCC;",
                               div(class = "card-body text-center text-muted", style = "padding: 120px 20px;",
                                   h4("Select a target on the left to view its progress.")
                               )
                           )
                         ),
                         conditionalPanel(
                           condition = sprintf("input['%s'] != null", ns("action_targets_table_rows_selected")),
                           fluidRow( 
                             column(width = 7,
                                    div(class = "card shadow-sm",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Progress Updates"),
                                        div(class = "card-body", 
                                            plotlyOutput(ns("act_progress_plot"), height = "250px"),
                                            hr(),
                                            DTOutput(ns("act_updates_table"))
                                        )
                                    )
                             ),
                             column(width = 5,
                                    div(class = "card shadow-sm mb-3",
                                        div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"),
                                        div(class = "card-body", uiOutput(ns("act_threats_ui")))
                                    ),
                                    div(class = "card shadow-sm",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Other Targets in this Action"),
                                        div(class = "card-body", uiOutput(ns("act_other_targets_ui")))
                                    )
                             )
                           )
                         )
                  )
                )
      ),
      
      # ==========================================
      # TAB 3: ALL ACTIONS DIRECTORY
      # ==========================================
      nav_panel("All Actions List",
                fluidRow(
                  column(width = 4,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header text-white", style = "background-color: #055A53;", "Global Action Directory"),
                             div(class = "card-body",
                                 DTOutput(ns("all_actions_table"))
                             )
                         )
                  ),
                  column(width = 8,
                         conditionalPanel(
                           condition = sprintf("input['%s'] == null", ns("all_actions_table_rows_selected")),
                           div(class = "card shadow-sm", style = "background-color: #FAFAFA; border: 2px dashed #CCCCCC;",
                               div(class = "card-body text-center text-muted", style = "padding: 120px 20px;",
                                   h4("Select an action on the left to view its details.")
                               )
                           )
                         ),
                         conditionalPanel(
                           condition = sprintf("input['%s'] != null", ns("all_actions_table_rows_selected")),
                           fluidRow(
                             column(width = 6,
                                    div(class = "card shadow-sm mb-3",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #0D67B8;", "Targets & Details"),
                                        div(class = "card-body", uiOutput(ns("all_act_targets_ui")))
                                    ),
                                    div(class = "card shadow-sm mb-3",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Mitigated Threats"),
                                        div(class = "card-body", uiOutput(ns("all_act_threats_ui")))
                                    )
                             ),
                             column(width = 6,
                                    div(class = "card shadow-sm mb-3",
                                        div(class = "card-header text-white fw-bold", style = "background-color: #EAB11E;", "Most Recent Update"),
                                        div(class = "card-body", uiOutput(ns("all_act_recent_update_ui")))
                                    )
                             )
                           )
                         )
                  )
                )
      ),
      
      # ==========================================
      # TAB 4: SYSTEM CONNECTIVITY
      # ==========================================
      nav_panel("System Connectivity Explorer",
                div(class = "mt-3",
                    div(class = "card shadow-sm mb-4",
                        div(class = "card-header text-white", style = "background-color: #AA5F40;", "Connectivity Filters"),
                        div(class = "card-body",
                            layout_columns(
                              selectInput(ns("conn_filter_type"), "View Connections for:", choices = c("All Targets", "Species Only", "Habitats Only")),
                              radioButtons(ns("conn_chart_choice"), "Select Visualization:", 
                                           choices = c("Sankey Flow" = "sankey", "Interactive Edge Bundling" = "bundling"), 
                                           inline = TRUE)
                            ),
                            p(em("Visualize how L2 Actions flow into Mitigated Threats and benefit specific Targets.", class="text-muted small"))
                        )
                    ),
                    div(class = "card shadow-sm",
                        div(class = "card-body", style = "min-height: 750px; display: flex; justify-content: center; overflow: hidden;",
                            
                            # THE FIX: Swapped to Plotly for Sankey
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'sankey'", ns("conn_chart_choice")),
                              plotlyOutput(ns("sankey_plot"), height = "700px", width = "100%")
                            ),
                            
                            # THE FIX: Swapped to ggiraph for Edge Bundling
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'bundling'", ns("conn_chart_choice")),
                              girafeOutput(ns("heb_plot"), width = "100%", height = "800px")
                            )
                        )
                    )
                )
      )
    )
  )
}

dashboard_server <- function(id, db, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------------------------------------
    # SHARED DROPDOWNS / TAB 1 & 2 LOGIC
    # ---------------------------------------------------------
    observe({
      db_sync_trigger() 
      tax_groups <- get_dash_tax_groups(db)
      updateSelectInput(session, "dash_tax_group", choices = c("Select group..." = "", tax_groups$groupname))
      habitats <- get_dash_major_habitats(db)
      updateSelectInput(session, "dash_major_hab", choices = c("Select habitat..." = "", habitats$majorhabitatname))
      l0_acts <- get_dash_l0_actions(db)
      updateSelectInput(session, "dash_l0", choices = c("Select L0..." = "", setNames(l0_acts$actionl0id, l0_acts$actionl0name)))
    })
    
    observeEvent(input$dash_tax_group, {
      req(input$dash_tax_group); query <- "SELECT DISTINCT s.speciesid, s.commonname FROM proj.species s JOIN track.specieshabitatactions sha ON s.speciesid = sha.speciesid JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE tg.groupname = $1 ORDER BY s.commonname"
      res <- dbGetQuery(db, query, params = list(input$dash_tax_group))
      updateSelectizeInput(session, "dash_species", choices = c("Choose species..." = "", setNames(res$speciesid, res$commonname)))
    })
    
    observeEvent(input$dash_major_hab, {
      req(input$dash_major_hab); query <- "SELECT DISTINCT hs.habitatsubtypeid, hs.habitatsubtypename FROM proj.habitatsubtypes hs JOIN track.specieshabitatactions sha ON hs.habitatsubtypeid = sha.habitatsubtypeid JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE mh.majorhabitatname = $1 ORDER BY hs.habitatsubtypename"
      res <- dbGetQuery(db, query, params = list(input$dash_major_hab))
      updateSelectizeInput(session, "dash_habitat", choices = c("Choose subtype..." = "", setNames(res$habitatsubtypeid, res$habitatsubtypename)))
    })
    
    targ_actions_data <- reactive({
      db_sync_trigger(); is_sp <- input$targ_type == "Species"; tid <- if(is_sp) input$dash_species else input$dash_habitat
      if (is.null(tid) || tid == "") return(data.frame())
      q <- if(is_sp) {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2name AS \"L2 Action\", COALESCE(sadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" 
         FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid WHERE sha.speciesid = $1"
      } else {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2name AS \"L2 Action\", COALESCE(hadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" 
         FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE sha.habitatsubtypeid = $1"
      }
      dbGetQuery(db, q, params = list(as.integer(tid)))
    })
    
    output$target_actions_table <- renderDT({
      df <- targ_actions_data(); if(nrow(df) == 0) return(datatable(data.frame(Message = "No targets selected."), rownames = F, options = list(dom = 't')))
      datatable(df, selection = "single", rownames = F, options = list(pageLength = 5, dom = 'tp', columnDefs = list(list(visible = F, targets = c(0, 1)))))
    })
    
    output$targ_threats_ui <- renderUI({
      req(input$target_actions_table_rows_selected); sha_id <- targ_actions_data()[input$target_actions_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.specieshabitatactionsid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(sha_id)))
      if(nrow(threats) == 0) return(p("No threats recorded."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(threats), function(i) tags$li(strong(threats$t_name[i]), br(), em(threats$justification[i]), class="mb-3")))
    })
    
    output$targ_other_targets_ui <- renderUI({
      req(input$target_actions_table_rows_selected); impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]; sha_id <- targ_actions_data()[input$target_actions_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 AND sha.specieshabitatactionsid != $2"
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id), as.integer(sha_id)))
      if(nrow(others) == 0) return(p("No other targets for this action."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(others), function(i) tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")")))
    })
    
    targ_updates_raw <- reactive({
      req(input$target_actions_table_rows_selected); impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]
      dbGetQuery(db, "SELECT actiondate::date AS \"Date\", stattype AS \"Metric\", stat AS \"Value\", comments AS \"Notes\" FROM track.actiontracking WHERE implementedactionid = $1 ORDER BY actiondate ASC", params = list(as.integer(impl_id)))
    })
    
    output$targ_progress_plot <- renderPlotly({
      df <- targ_updates_raw(); if (nrow(df) == 0) return(plotly_empty())
      plot_df <- df %>% group_by(Metric) %>% mutate(PlotValue = if_else(Metric == "Count", cumsum(Value), Value)) %>% ungroup() %>%
        mutate(HoverText = if_else(Metric == "Count", paste0("Date: ", Date, "<br>Cumulative: ", PlotValue), paste0("Date: ", Date, "<br>Percentage: ", PlotValue, "%")))
      p <- ggplot(plot_df, aes(x = Date, y = PlotValue, color = Metric, group = Metric, text = HoverText)) + geom_line(linewidth = 1) + geom_point(size = 2)
      met <- unique(plot_df$Metric); if(length(met) > 1) p <- p + facet_wrap(~Metric, scales = "free_y", ncol = 1) else p <- p + labs(y = met[1])
      if(met[1] == "Percentage") p <- p + scale_y_continuous(limits = c(0, max(100, max(plot_df$PlotValue))))
      p <- p + scale_color_manual(values = c("Count" = "#0D67B8", "Percentage" = "#EAB11E")) + theme_classic() + theme(legend.position = "none", text = element_text(color = "black"))
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified") %>% config(displayModeBar = F)
    })
    
    output$targ_updates_table <- renderDT({
      df <- targ_updates_raw() %>% arrange(desc(Date)); if(nrow(df) == 0) return(datatable(data.frame(Message="No progress logs."), rownames=F, options=list(dom='t')))
      datatable(df, selection = 'single', rownames = F, options = list(pageLength = 4, dom = 'tp')) 
    })
    
    # ---------------------------------------------------------
    # TAB 2 LOGIC (EXPLORE BY ACTION)
    # ---------------------------------------------------------
    observeEvent(input$dash_l0, {
      req(input$dash_l0); q <- "SELECT DISTINCT l1.actionl1id, l1.actionl1code || '. ' || l1.actionl1name AS n FROM proj.l1_actions l1 JOIN proj.l2_actions l2 ON l1.actionl1id = l2.actionl1id JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l1.actionl0id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l0)); updateSelectInput(session, "dash_l1", choices = c("Select L1..." = "", setNames(res$actionl1id, res$n)))
    })
    
    observeEvent(input$dash_l1, {
      req(input$dash_l1); q <- "SELECT DISTINCT l2.actionl2id, l2.actionl2code || '. ' || l2.actionl2name AS n FROM proj.l2_actions l2 JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l2.actionl1id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l1)); updateSelectInput(session, "dash_l2", choices = c("Select L2..." = "", setNames(res$actionl2id, res$n)))
    })
    
    act_targets_data <- reactive({
      db_sync_trigger(); if (is.null(input$dash_l2) || input$dash_l2 == "") return(data.frame())
      q <- "SELECT ia.implementedactionid, sha.specieshabitatactionsid, CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS \"Target Name\", CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS \"Type\", l2.actionl2name AS \"L2 Action\", COALESCE(sadd.\"Meaningful.Details\", hadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.status AS \"Status\", ia.timeframe AS \"Timeframe\" FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE ia.actionl2id = $1"
      dbGetQuery(db, q, params = list(as.integer(input$dash_l2)))
    })
    
    output$action_targets_table <- renderDT({
      df <- act_targets_data(); if(nrow(df) == 0) return(datatable(data.frame(Message = "No action selected."), rownames = F, options = list(dom = 't')))
      datatable(df, selection = "single", rownames = F, options = list(pageLength = 5, dom = 'tp', columnDefs = list(list(visible = F, targets = c(0, 1)))))
    })
    
    output$act_threats_ui <- renderUI({
      req(input$action_targets_table_rows_selected); sha_id <- act_targets_data()[input$action_targets_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.specieshabitatactionsid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(sha_id)))
      if(nrow(threats) == 0) return(p("No threats recorded."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(threats), function(i) tags$li(strong(threats$t_name[i]), br(), em(threats$justification[i]), class="mb-3")))
    })
    
    output$act_other_targets_ui <- renderUI({
      req(input$action_targets_table_rows_selected); impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]; sha_id <- act_targets_data()[input$action_targets_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 AND sha.specieshabitatactionsid != $2"
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id), as.integer(sha_id)))
      if(nrow(others) == 0) return(p("No other targets for this action."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(others), function(i) tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")")))
    })
    
    act_updates_raw <- reactive({
      req(input$action_targets_table_rows_selected); impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      dbGetQuery(db, "SELECT actiondate::date AS \"Date\", stattype AS \"Metric\", stat AS \"Value\", comments AS \"Notes\" FROM track.actiontracking WHERE implementedactionid = $1 ORDER BY actiondate ASC", params = list(as.integer(impl_id)))
    })
    
    output$act_progress_plot <- renderPlotly({
      df <- act_updates_raw(); if (nrow(df) == 0) return(plotly_empty())
      plot_df <- df %>% group_by(Metric) %>% mutate(PlotValue = if_else(Metric == "Count", cumsum(Value), Value)) %>% ungroup() %>%
        mutate(HoverText = if_else(Metric == "Count", paste0("Date: ", Date, "<br>Cumulative: ", PlotValue), paste0("Date: ", Date, "<br>Percentage: ", PlotValue, "%")))
      p <- ggplot(plot_df, aes(x = Date, y = PlotValue, color = Metric, group = Metric, text = HoverText)) + geom_line(linewidth = 1) + geom_point(size = 2)
      met <- unique(plot_df$Metric); if(length(met) > 1) p <- p + facet_wrap(~Metric, scales = "free_y", ncol = 1) else p <- p + labs(y = met[1])
      if(met[1] == "Percentage") p <- p + scale_y_continuous(limits = c(0, max(100, max(plot_df$PlotValue))))
      p <- p + scale_color_manual(values = c("Count" = "#0D67B8", "Percentage" = "#EAB11E")) + theme_classic() + theme(legend.position = "none", text = element_text(color = "black"))
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified") %>% config(displayModeBar = F)
    })
    
    output$act_updates_table <- renderDT({
      df <- act_updates_raw() %>% arrange(desc(Date)); if(nrow(df) == 0) return(datatable(data.frame(Message="No progress logs."), rownames=F, options=list(dom='t')))
      datatable(df, selection = 'single', rownames = F, options = list(pageLength = 4, dom = 'tp'))
    })
    
    # ---------------------------------------------------------
    # TAB 3 LOGIC (ALL ACTIONS DIRECTORY)
    # ---------------------------------------------------------
    all_actions_data <- reactive({
      db_sync_trigger(); q <- "SELECT ia.implementedactionid, l2.actionl2name AS \"L2 Action\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" FROM track.implementedactions ia JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id ORDER BY CASE WHEN ia.status = 'Completed' THEN 1 WHEN ia.status = 'In Progress' THEN 2 WHEN ia.status = 'Planned' THEN 3 ELSE 4 END ASC, ia.createdon DESC"
      dbGetQuery(db, q)
    })
    
    output$all_actions_table <- renderDT({
      datatable(all_actions_data(), selection = "single", rownames = F, options = list(pageLength = 10, dom = 'ftip', columnDefs = list(list(visible = F, targets = 0))))
    })
    
    output$all_act_targets_ui <- renderUI({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type, COALESCE(sadd.\"Meaningful.Details\", hadd.\"Meaningful.Details\", 'None') AS detail_text FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE sha.implementedactionid = $1"
      res <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(res) == 0) return(p("No targets mapped."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(res), function(i) tags$li(strong(res$target_name[i]), " (", res$target_type[i], ")", br(), em("Detail: ", res$detail_text[i]), class = "mb-2")))
    })
    
    output$all_act_threats_ui <- renderUI({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT DISTINCT l2.threatl2code || '. ' || l2.threatl2name AS t_name FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.implementedactionid = $1"
      res <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(res) == 0) return(p("No threats."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(res), function(i) tags$li(res$t_name[i])))
    })
    
    output$all_act_recent_update_ui <- renderUI({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT actiondate::date AS d, stattype AS m, stat AS v, comments AS n, COALESCE(p.first_name || ' ' || p.last_name, a.createdby) AS u FROM track.actiontracking a LEFT JOIN public.profiles p ON a.createdby = p.id::text WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC LIMIT 1"
      res <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(res) == 0) return(p(em("No updates logged.")))
      tagList(p(strong("Date: "), res$d, br(), strong(paste0("Metric (", res$m, "): ")), res$v, br(), strong("User: "), res$u), p(strong("Notes: "), br(), em(res$n)))
    })
    
    # ---------------------------------------------------------
    # TAB 4 LOGIC (SYSTEM CONNECTIVITY)
    # ---------------------------------------------------------
    links_data <- reactive({
      db_sync_trigger()
      query <- "
        SELECT ia.actionl2id, l2a.actionl2name AS source_name, ta.threatl2id, l2t.threatl2name AS threat_name,
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name,
          CASE WHEN sha.specieshabitat = TRUE THEN tg.groupname ELSE mh.majorhabitatname END AS group_context,
          CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type
        FROM track.implementedactions ia
        JOIN proj.l2_actions l2a ON ia.actionl2id = l2a.actionl2id
        JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        JOIN track.threatsaddressed ta ON sha.specieshabitatactionsid = ta.specieshabitatactionsid
        JOIN proj.l2_threats l2t ON ta.threatl2id = l2t.threatl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid
      "
      df <- dbGetQuery(db, query)
      if (input$conn_filter_type == "Species Only") df <- df %>% filter(target_type == "Species")
      if (input$conn_filter_type == "Habitats Only") df <- df %>% filter(target_type == "Habitat")
      return(df)
    })
    
    # THE FIX: Upgraded to Plotly Sankey
    output$sankey_plot <- renderPlotly({
      df <- links_data(); req(nrow(df) > 0)
      
      # Step 1: Create distinct lists but track what category they belong to
      l1 <- df %>% group_by(source_name, threat_name) %>% summarize(value = n(), .groups = "drop") %>% rename(source = source_name, target = threat_name) %>% mutate(src_type="Action", tgt_type="Threat")
      l2 <- df %>% group_by(threat_name, target_name) %>% summarize(value = n(), .groups = "drop") %>% rename(source = threat_name, target = target_name) %>% mutate(src_type="Threat", tgt_type="Target")
      
      # Step 2: Build a master node dictionary (Prefix guarantees uniqueness under the hood)
      all_sources <- bind_rows(
        l1 %>% select(name_raw = source, type = src_type),
        l2 %>% select(name_raw = source, type = src_type)
      )
      all_targets <- bind_rows(
        l1 %>% select(name_raw = target, type = tgt_type),
        l2 %>% select(name_raw = target, type = tgt_type)
      )
      
      nodes_df <- bind_rows(all_sources, all_targets) %>% 
        distinct() %>%
        mutate(
          unique_id = paste0(type, "_", name_raw), # Hidden unique ID
          color = case_when(
            type == "Action" ~ "#0D67B8",
            type == "Threat" ~ "#AA5F40",
            type == "Target" ~ "#055A53"
          )
        )
      
      # Add 0-based index for plotly
      nodes_df$idx <- 0:(nrow(nodes_df) - 1)
      
      # Step 3: Map links to the 0-based index
      links <- bind_rows(l1, l2) %>%
        mutate(
          source_uid = paste0(src_type, "_", source),
          target_uid = paste0(tgt_type, "_", target)
        )
      links$source_id <- nodes_df$idx[match(links$source_uid, nodes_df$unique_id)]
      links$target_id <- nodes_df$idx[match(links$target_uid, nodes_df$unique_id)]
      
      # Step 4: Render the Plotly Sankey
      plot_ly(
        type = "sankey", orientation = "h",
        node = list(
          label = nodes_df$name_raw, # Clean display names!
          color = nodes_df$color,
          pad = 15, thickness = 20,
          line = list(color = "black", width = 0.5),
          hovertemplate = "<b>%{label}</b><br>Total Connections: %{value}<extra></extra>"
        ),
        link = list(
          source = links$source_id,
          target = links$target_id,
          value = links$value,
          color = "rgba(200, 200, 200, 0.4)",
          hovertemplate = "<b>Source:</b> %{source.label}<br><b>Target:</b> %{target.label}<extra></extra>"
        )
      ) %>% layout(
        font = list(size = 12),
        margin = list(t = 60, b = 20, l = 20, r = 20),
        # THE FIX: Add explicit column headers
        annotations = list(
          list(x = 0, y = 1.05, text = "<b>Actions</b>", showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "left", font = list(size = 16)),
          list(x = 0.5, y = 1.05, text = "<b>Mitigated Threats</b>", showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "center", font = list(size = 16)),
          list(x = 1, y = 1.05, text = "<b>Targets</b>", showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "right", font = list(size = 16))
        )
      ) %>% config(displayModeBar = FALSE)
    })
    
    # THE FIX: Upgraded to ggiraph for interactive edge bundling
    output$heb_plot <- renderGirafe({
      df <- links_data(); req(nrow(df) > 0)
      
      df_heb <- df %>%
        mutate(source_name = paste("Action:", source_name),
               threat_name = paste("Threat:", threat_name),
               target_name = paste("Target:", target_name))
      
      edges_at <- df_heb %>% select(from = source_name, to = threat_name) %>% distinct()
      edges_tt <- df_heb %>% select(from = threat_name, to = target_name) %>% distinct()
      connections <- bind_rows(edges_at, edges_tt)
      
      hier_L1 <- data.frame(from = "Root", to = c("Actions", "Threats", "Targets"))
      hier_act <- data.frame(from = "Actions", to = unique(df_heb$source_name))
      hier_thr <- data.frame(from = "Threats", to = unique(df_heb$threat_name))
      hier_targ <- data.frame(from = "Targets", to = unique(df_heb$target_name))
      
      hierarchy <- bind_rows(hier_L1, hier_act, hier_thr, hier_targ) %>% distinct()
      
      vertices <- data.frame(name = unique(c("Root", hierarchy$from, hierarchy$to)))
      vertices <- vertices %>%
        mutate(
          group = case_when(
            name %in% hier_act$to ~ "Action",
            name %in% hier_thr$to ~ "Threat",
            name %in% hier_targ$to ~ "Target",
            TRUE ~ "Root"
          ),
          clean_name = sub(".*: ", "", name),
          # Wrap labels to 25 chars so they don't stick out infinitely
          wrapped_name = stringr::str_wrap(clean_name, width = 25), 
          # Create HTML hover text
          hover_text = paste0("<b>", group, "</b><br>", clean_name)
        )
      
      mygraph <- graph_from_data_frame(hierarchy, vertices = vertices)
      from_idx <- match(connections$from, vertices$name)
      to_idx <- match(connections$to, vertices$name)
      
      p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
        geom_conn_bundle(data = get_con(from = from_idx, to = to_idx),
                         alpha = 0.2, width = 0.8, color = "#AA5F40", tension = 0.7) +
        # THE FIX: Use standard ggiraph geoms and map x/y manually, filtering for leaves directly in the data argument
        geom_point_interactive(aes(x = x, y = y, color = group, tooltip = hover_text, data_id = name), 
                               data = function(d) subset(d, leaf), size = 3) +
        # Interactive Text
        geom_text_interactive(aes(x = x * 1.08, y = y * 1.08, 
                                  label = wrapped_name,
                                  tooltip = hover_text,
                                  data_id = name,
                                  angle = ifelse(node_angle(x,y) > 90 & node_angle(x,y) < 270, node_angle(x,y) + 180, node_angle(x,y)),
                                  hjust = ifelse(node_angle(x,y) > 90 & node_angle(x,y) < 270, 1, 0)),
                              data = function(d) subset(d, leaf),
                              size = 3.5, color = "#212529", fontface = "bold") +
        scale_color_manual(values = c("Action" = "#0D67B8", "Threat" = "#AA5F40", "Target" = "#055A53")) +
        theme_void() +
        theme(legend.position = "bottom", 
              legend.title = element_blank(),
              legend.text = element_text(size = 12, face = "bold"),
              plot.margin = margin(80, 80, 80, 80)) +
        coord_cartesian(clip = "off")
      
      # Convert to ggiraph widget
      girafe(ggobj = p, width_svg = 12, height_svg = 12,
             options = list(
               opts_hover(css = "fill:#EAB11E;stroke:black;stroke-width:2px;cursor:pointer;"),
               opts_tooltip(css = "background-color:white;color:black;padding:10px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);")
             ))
    })
    
  }) 
}