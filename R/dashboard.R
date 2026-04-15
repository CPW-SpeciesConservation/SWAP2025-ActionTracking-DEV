library(memoise)
library(cachem)
library(ggplot2)
library(plotly)
library(ggiraph) 
library(stringr) 
library(dplyr)
library(igraph)
library(ggraph)
library(heatmaply)

# "global" cache of track schema data
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
      
      # TAB 1: EXPLORE BY Species/Habitat (sometimes referred to collectively as "target")
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
                                 h6("Tracked Actions for Selected Species/Habitat", class = "fw-bold text-muted"),
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
      
      # TAB 2: EXPLORE BY ACTION
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
                                 # h6("Actions Table", class = "fw-bold text-muted"),
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
      
   
      # TAB 3: ALL ACTIONS interactive table
      nav_panel("All Actions List",
                fluidRow(
                  column(width = 4,
                         div(class = "card shadow-sm mb-3",
                             div(class = "card-header text-white", style = "background-color: #055A53;", "Tracked Actions"),
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
                                        div(class = "card-header text-white fw-bold", style = "background-color: #0D67B8;", "Targeted Species & Habitats"),
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
      
      # TAB 4: Visulization
      nav_panel("Visualizing Actions, Species/Habitats, & Threats Connections",
                div(class = "mt-3",
                    div(class = "card shadow-sm mb-4",
                        div(class = "card-header text-white", style = "background-color: #AA5F40;", "Filters and Figures"),
                        div(class = "card-body",
                            layout_columns(
                              selectInput(ns("conn_filter_type"), "Filter:", choices = c("All", "Species Only", "Habitats Only")),
                              radioButtons(ns("conn_chart_choice"), "Select Visualization:", 
                                           choices = c("Sankey Chart" = "sankey", 
                                                       "Chord Graph" = "bipartite",
                                                       "Heatmap" = "heatmap"), 
                                           inline = TRUE)
                            ),
                            p(em("Visualize exactly how Conservation Actions flow through specific Species/Habitats to mitigate Threats.", class="text-muted small"))
                        )
                    ),
                    div(class = "card shadow-sm",
                        div(class = "card-body", style = "min-height: 750px; display: flex; justify-content: center; overflow: hidden;",
                            
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'sankey'", ns("conn_chart_choice")),
                              plotlyOutput(ns("sankey_plot"), height = "700px", width = "100%")
                            ),
                            
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'bipartite'", ns("conn_chart_choice")),
                              girafeOutput(ns("bipartite_plot"), width = "100%", height = "750px")
                            ),
                            
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'heatmap'", ns("conn_chart_choice")),
                              plotlyOutput(ns("heatmap_plot"), height = "700px", width = "100%")
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

    # SHARED DROPDOWNS / TAB 1 & 2 LOGIC
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
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", COALESCE(sadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" 
         FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid WHERE sha.speciesid = $1"
      } else {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", COALESCE(hadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" 
         FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE sha.habitatsubtypeid = $1"
      }
      dbGetQuery(db, q, params = list(as.integer(tid)))
    })
    
    output$target_actions_table <- renderDT({
      df <- targ_actions_data(); if(nrow(df) == 0) 
        return(datatable(data.frame(Message = "No species/habitats selected."), 
                         rownames = F, options = list(dom = 't')))
      datatable(df, selection = "single", rownames = F, 
                options = list(dom = 't',
                               paging= FALSE,
                               scrollY="250px",
                               scrollCollapse=TRUE,
                               columnDefs = list(list(visible = F, targets = c(0, 1)))))
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
      datatable(df, selection = 'single', rownames = F, options = list(
        dom = 't',
        paging = FALSE,          
        scrollY = "250px",       
        scrollCollapse = TRUE,   
        info = FALSE)) 
    })
    
  
    # TAB 2 LOGIC (EXPLORE BY ACTION)
    
    observeEvent(input$dash_l0, {
      req(input$dash_l0); q <- "SELECT DISTINCT l1.actionl1id, l1.actionl1code || '. ' || l1.actionl1name AS n FROM proj.l1_actions l1 JOIN proj.l2_actions l2 ON l1.actionl1id = l2.actionl1id JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l1.actionl0id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l0)); updateSelectInput(session, "dash_l1", choices = c("Select L1..." = "", setNames(res$actionl1id, res$n)))
    })
    
    observeEvent(input$dash_l1, {
      req(input$dash_l1); q <- "SELECT DISTINCT l2.actionl2id, l2.actionl2code || '. ' || l2.actionl2name AS n FROM proj.l2_actions l2 JOIN track.implementedactions ia ON l2.actionl2id = ia.actionl2id WHERE l2.actionl1id = $1 ORDER BY n"
      res <- dbGetQuery(db, q, params = list(input$dash_l1)); updateSelectInput(session, "dash_l2", choices = c("Select L2..." = "", setNames(res$actionl2id, res$n)))
    })
    
    act_targets_data <- reactive({
      db_sync_trigger()
      if (is.null(input$dash_l2) || input$dash_l2 == "") return(data.frame())
      
      q <- "SELECT 
              ia.implementedactionid, 
              l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", 
              ia.status AS \"Status\", 
              ia.timeframe AS \"Timeframe\",
              STRING_AGG(CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END, ', ') AS \"Included Targets\"
            FROM track.implementedactions ia 
            JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid 
            JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id 
            LEFT JOIN proj.species s ON sha.speciesid = s.speciesid 
            LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid 
            WHERE ia.actionl2id = $1
            GROUP BY ia.implementedactionid, l2.actionl2code, l2.actionl2name, ia.status, ia.timeframe
            ORDER BY ia.implementedactionid DESC"
      
      dbGetQuery(db, q, params = list(as.integer(input$dash_l2)))
    })
    
    output$action_targets_table <- renderDT({
      df <- act_targets_data()
      if(nrow(df) == 0) return(datatable(data.frame(Message = "No action selected."), rownames = F, options = list(dom = 't')))
      
      datatable(df, selection = "single", rownames = F, options = list(
        dom = 'ft', 
        paging = FALSE,          
        scrollY = "250px",      
        scrollCollapse = TRUE,   
        info = FALSE,
        columnDefs = list(list(visible = F, targets = 0))))
    })
    
    output$act_threats_ui <- renderUI({
      req(input$action_targets_table_rows_selected)
      impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT 
              l2.threatl2code || '. ' || l2.threatl2name AS t_name, 
              ta.justification,
              CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_label
            FROM track.threatsaddressed ta 
            JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid 
            JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id 
            LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
            LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
            WHERE sha.implementedactionid = $1
            ORDER BY target_label ASC, t_name ASC"
      
      threats <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(threats) == 0) return(p("No threats recorded."))
      
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(threats), function(i) {
        tags$li(
          strong(threats$t_name[i]), 
          span(style="color: #0D67B8; font-size: 0.9em; font-weight: bold;", paste0(" [", threats$target_label[i], "]")),
          br(), 
          em(threats$justification[i]), 
          class="mb-3"
        )
      }))
    })
    
    output$act_other_targets_ui <- renderUI({
      req(input$action_targets_table_rows_selected)
      impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      
      q <- "SELECT 
              CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, 
              CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type 
            FROM track.specieshabitatactions sha 
            LEFT JOIN proj.species s ON sha.speciesid = s.speciesid 
            LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid 
            WHERE sha.implementedactionid = $1
            ORDER BY target_type DESC, target_name ASC"
      
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(others) == 0) return(p("No species/habitats assigned."))
      
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(others), function(i) {
        tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")")
      }))
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
      datatable(df, selection = 'single', rownames = F, options = list(
        dom = 't',
        paging = FALSE,          
        scrollY = "250px",       
        scrollCollapse = TRUE,   
        info = FALSE))
    })
    
 
    # TAB 3 LOGIC (ALL ACTIONS Table)
   
    all_actions_data <- reactive({
      db_sync_trigger(); q <- "SELECT ia.implementedactionid, l2.actionl2name AS \"L2 Action\", ia.timeframe AS \"Timeframe\", ia.status AS \"Status\" FROM track.implementedactions ia JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id ORDER BY CASE WHEN ia.status = 'Completed' THEN 1 WHEN ia.status = 'In Progress' THEN 2 WHEN ia.status = 'Planned' THEN 3 ELSE 4 END ASC, ia.createdon DESC"
      dbGetQuery(db, q)
    })
    
    output$all_actions_table <- renderDT({
      datatable(all_actions_data(), selection = "single", rownames = F, options = list(
        dom = 'ftip',
        paging = FALSE,        
        scrollY = "calc(100vh - 250px)",       
        scrollCollapse = TRUE,   
        info = FALSE,
        columnDefs = list(list(visible = F, targets = 0))))
    })
    
    output$all_act_targets_ui <- renderUI({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type, COALESCE(sadd.\"Meaningful.Details\", hadd.\"Meaningful.Details\", 'None') AS detail_text FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE sha.implementedactionid = $1"
      res <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(res) == 0) return(p("No species/habitats mapped."))
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
    
 
    # TAB 4 LOGIC (Figures)
   
    links_data <- reactive({
      db_sync_trigger()
      
      query <- "
        SELECT 
          l0a.actionl0id, TRIM(COALESCE(l0a.actionl0name, 'Unknown')) AS actionl0name, 
          l1a.actionl1id, TRIM(COALESCE(l1a.actionl1name, 'Unknown')) AS actionl1name, 
          l2a.actionl2id, TRIM(l2a.actionl2code) AS actionl2code, TRIM(l2a.actionl2name) AS source_name,
          l0t.threatl0id, TRIM(COALESCE(l0t.threatl0name, 'Unknown')) AS threatl0name, 
          l1t.threatl1id, TRIM(COALESCE(l1t.threatl1name, 'Unknown')) AS threatl1name, 
          l2t.threatl2id, TRIM(l2t.threatl2code) AS threatl2code, TRIM(l2t.threatl2name) AS threat_name,
          CASE WHEN sha.specieshabitat = TRUE THEN TRIM(s.commonname) ELSE TRIM(hs.habitatsubtypename) END AS target_name,
          CASE WHEN sha.specieshabitat = TRUE THEN TRIM(tg.groupname) ELSE TRIM(mh.majorhabitatname) END AS group_context,
          CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type
        FROM track.implementedactions ia
        JOIN proj.l2_actions l2a ON ia.actionl2id = l2a.actionl2id
        LEFT JOIN proj.l1_actions l1a ON l2a.actionl1id = l1a.actionl1id
        LEFT JOIN proj.l0_actions l0a ON l1a.actionl0id = l0a.actionl0id
        JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        JOIN track.threatsaddressed ta ON sha.specieshabitatactionsid = ta.specieshabitatactionsid
        JOIN proj.l2_threats l2t ON ta.threatl2id = l2t.threatl2id
        LEFT JOIN proj.l1_threats l1t ON l2t.threatl1id = l1t.threatl1id
        LEFT JOIN proj.l0_threats l0t ON l1t.threatl0id = l0t.threatl0id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid
      "
      df <- dbGetQuery(db, query)
      
      if (nrow(df) > 0) {
        df <- df %>%
          filter(
            !actionl0name %in% c("D. None", "Unknown", "None"), 
            actionl2code != "11.1",                          
            !grepl("\\.9$", threatl2code),
            !threatl0name %in% c("Unknown Threats", "Unknown", "None"),
            threatl1id != 4
          )
      }
      if (input$conn_filter_type == "Species Only") df <- df %>% filter(target_type == "Species")
      if (input$conn_filter_type == "Habitats Only") df <- df %>% filter(target_type == "Habitat")
      
      return(df)
    })
    
    # --- 4A. Plotly Sankey (Action -> Target -> Threat) ---
    output$sankey_plot <- renderPlotly({
      df <- links_data(); req(nrow(df) > 0)
      
      df <- df %>% mutate(
        source_name_full = paste(actionl2code, source_name, sep=" "),
        threat_name_full = paste(threatl2code, threat_name, sep=" ")
      )
      
      l1 <- df %>% group_by(source_name_full, target_name, target_type) %>% summarize(value = n(), .groups = "drop") %>% 
        rename(source = source_name_full, target = target_name) %>% mutate(src_type="Action", tgt_type=target_type)
      
      l2 <- df %>% group_by(target_name, threat_name_full, target_type) %>% summarize(value = n(), .groups = "drop") %>% 
        rename(source = target_name, target = threat_name_full) %>% mutate(src_type=target_type, tgt_type="Threat")
      
      acts <- df %>% select(name_raw = source_name_full, l0=actionl0id, l1=actionl1id, l2=actionl2id) %>% distinct() %>% arrange(l0, l1, l2) %>% mutate(type="Action", x=0.01)
      acts$y <- seq(0.01, 0.99, length.out = nrow(acts))
      
      targs <- df %>% select(name_raw = target_name, target_type, group_context) %>% distinct() %>% arrange(desc(target_type), group_context, name_raw) %>% mutate(type=target_type, x=0.5)
      targs$y <- seq(0.01, 0.99, length.out = nrow(targs))
      
      thrs <- df %>% select(name_raw = threat_name_full, l0=threatl0id, l1=threatl1id, l2=threatl2id) %>% distinct() %>% arrange(l0, l1, l2) %>% mutate(type="Threat", x=0.99)
      thrs$y <- seq(0.01, 0.99, length.out = nrow(thrs))
      
      nodes_df <- bind_rows(acts, targs, thrs) %>%
        mutate(
          unique_id = paste0(type, "_", name_raw), 
          idx = 0:(n()-1),
          color = case_when(type == "Action" ~ "#0D67B8", type == "Threat" ~ "#AA5F40", type == "Species" ~ "#055A53", type == "Habitat" ~ "#43956F")
        )
      
      links <- bind_rows(l1, l2) %>% mutate(source_uid = paste0(src_type, "_", source), target_uid = paste0(tgt_type, "_", target))
      links$source_id <- nodes_df$idx[match(links$source_uid, nodes_df$unique_id)]
      links$target_id <- nodes_df$idx[match(links$target_uid, nodes_df$unique_id)]
      
      plot_ly(
        type = "sankey", orientation = "h", valueformat = "d", arrangement = "fixed",
        node = list(label = nodes_df$name_raw, color = nodes_df$color, x = nodes_df$x, y = nodes_df$y, pad = 15, thickness = 20, line = list(color = "black", width = 0.5), hovertemplate = "<b>%{label}</b><br>Total Links: %{value:d}<extra></extra>"),
        link = list(source = links$source_id, target = links$target_id, value = links$value, color = "rgba(200, 200, 200, 0.4)", hovertemplate = "<b>Source:</b> %{source.label}<br><b>Destination:</b> %{target.label}<br><b>Connections:</b> %{value:d}<extra></extra>")
      ) %>% layout(
        font = list(size = 12), margin = list(t = 60, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0, y = 1.05, text = "<b>Actions</b>", showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "left", font = list(size = 16)),
          list(x = 0.5, y = 1.05, text = "<b>Species/Habitats</b>", showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "center", font = list(size = 16)),
          list(x = 1, y = 1.05, text = "<b>Mitigated Threats</b>", showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "right", font = list(size = 16))
        )
      ) %>% config(displayModeBar = FALSE)
    })
    
    # --- 4B. Interactive Circular Hierarchy (Sunburst Style) ---
    output$bipartite_plot <- renderGirafe({
      df <- links_data()
      
      # 1. Fetch Full Lexicon with correct nesting
      q_act <- "SELECT l0a.actionl0id, TRIM(COALESCE(l0a.actionl0name, 'Unknown')) AS l0, l1a.actionl1id, TRIM(COALESCE(l1a.actionl1name, 'Unknown')) AS l1, l2a.actionl2id, TRIM(l2a.actionl2code) AS code, TRIM(l2a.actionl2name) AS name FROM proj.l2_actions l2a LEFT JOIN proj.l1_actions l1a ON l2a.actionl1id = l1a.actionl1id LEFT JOIN proj.l0_actions l0a ON l1a.actionl0id = l0a.actionl0id"
      q_thr <- "SELECT l0t.threatl0id, TRIM(COALESCE(l0t.threatl0name, 'Unknown')) AS l0, l1t.threatl1id, TRIM(COALESCE(l1t.threatl1name, 'Unknown')) AS l1, l2t.threatl2id, TRIM(l2t.threatl2code) AS code, TRIM(l2t.threatl2name) AS name FROM proj.l2_threats l2t LEFT JOIN proj.l1_threats l1t ON l2t.threatl1id = l1t.threatl1id LEFT JOIN proj.l0_threats l0t ON l1t.threatl0id = l0t.threatl0id"
      
      # Force perfectly numerical sort
      actions <- dbGetQuery(db, q_act) %>% filter(!l0 %in% c("D. None", "Unknown", "None"), code != "11.1") %>% 
        mutate(major_code = suppressWarnings(as.numeric(sub("\\..*", "", code))), minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", code)))) %>%
        arrange(l0, major_code, minor_code) %>% mutate(type = "Action")
      
      threats <- dbGetQuery(db, q_thr) %>% filter(!grepl("\\.9$", code), !l0 %in% c("Unknown Threats", "Unknown", "None"), threatl1id != 4) %>% 
        mutate(major_code = suppressWarnings(as.numeric(sub("\\..*", "", code))), minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", code)))) %>%
        arrange(l0, major_code, minor_code) %>% mutate(type = "Threat")
      
      # 2. Calculate Angular Positions
      n_act <- nrow(actions)
      act_angles <- seq(105 * pi/180, 255 * pi/180, length.out = max(2, n_act))
      actions <- actions %>% mutate(angle = act_angles, x = cos(angle), y = sin(angle), raw_deg = angle * 180 / pi, rad_angle = raw_deg - 180, hjust = 1)
      
      n_thr <- nrow(threats)
      thr_angles <- seq(75 * pi/180, -75 * pi/180, length.out = max(2, n_thr))
      threats <- threats %>% mutate(angle = thr_angles, x = cos(angle), y = sin(angle), raw_deg = angle * 180 / pi, rad_angle = raw_deg, hjust = 0)
      
      nodes_df <- bind_rows(actions, threats) %>% mutate(safe_id = paste0("node_", row_number()), full_text = paste(code, name, sep=" "))
      
      # 3. Dynamic Solid Radial Spokes for L1 separators
      separators <- data.frame()
      if (n_act > 1) {
        act_bounds <- actions %>% mutate(idx = row_number()) %>% group_by(l1) %>% summarize(max_idx = max(idx), .groups="drop") %>% arrange(max_idx) %>% filter(max_idx < nrow(actions))
        act_sep_angles <- if(nrow(act_bounds) > 0) (actions$angle[act_bounds$max_idx] + actions$angle[act_bounds$max_idx + 1]) / 2 else numeric(0)
        separators <- rbind(separators, data.frame(angle = act_sep_angles, type="Action"))
      }
      if (n_thr > 1) {
        thr_bounds <- threats %>% mutate(idx = row_number()) %>% group_by(l1) %>% summarize(max_idx = max(idx), .groups="drop") %>% arrange(max_idx) %>% filter(max_idx < nrow(threats))
        thr_sep_angles <- if(nrow(thr_bounds) > 0) (threats$angle[thr_bounds$max_idx] + threats$angle[thr_bounds$max_idx + 1]) / 2 else numeric(0)
        separators <- rbind(separators, data.frame(angle = thr_sep_angles, type="Threat"))
      }
      
      separators <- separators %>% mutate(x_in = 0.95 * cos(angle), y_in = 0.95 * sin(angle), x_out = 2.10 * cos(angle), y_out = 2.10 * sin(angle))
      
      # 4. Ring 2: L1 Labels (Placed between spokes)
      l1_labels <- bind_rows(
        actions %>% group_by(l1) %>% summarize(angle = mean(angle), .groups="drop") %>% mutate(r = 1.35, text_angle = angle * 180/pi - 180, hjust=1),
        threats %>% group_by(l1) %>% summarize(angle = mean(angle), .groups="drop") %>% mutate(r = 1.35, text_angle = angle * 180/pi, hjust=0)
      )
      
      # 5. Ring 3: Custom L0 Arcs (Brackets)
      l0_arcs <- bind_rows(
        actions %>% group_by(l0) %>% summarize(start = min(angle), end = max(angle), .groups="drop") %>% mutate(type="Action"),
        threats %>% group_by(l0) %>% summarize(start = min(angle), end = max(angle), .groups="drop") %>% mutate(type="Threat")
      ) %>% mutate(
        label = stringr::str_wrap(l0, 25),
        raw_mid = (start + end) / 2, 
        raw_deg = raw_mid * 180 / pi,
        r_arc = 2.15,
        r_label = 2.35
      )
      
      arc_df <- bind_rows(lapply(1:nrow(l0_arcs), function(i) {
        a_seq <- seq(l0_arcs$start[i], l0_arcs$end[i], length.out = 50)
        data.frame(x = l0_arcs$r_arc[i] * cos(a_seq), y = l0_arcs$r_arc[i] * sin(a_seq), group = l0_arcs$l0[i])
      }))
      
      l0_labels_df <- l0_arcs %>% mutate(
        text_angle = case_when(
          type == "Action" ~ raw_deg - 90,
          type == "Threat" & raw_deg >= 0 ~ raw_deg - 90, 
          type == "Threat" & raw_deg < 0 ~ raw_deg + 90   
        ),
        hjust = 0.5
      )
      
      # 6. Ring 4: Master Background Watermarks
      bg_labels <- data.frame(x = c(-3.1, 3.1), y = c(0, 0), label = c("ACTIONS", "THREATS"), angle = c(90, 270))
      
      # 7. Map the active connections
      edges_df <- data.frame()
      if(nrow(df) > 0) {
        edges_df <- df %>%
          left_join(nodes_df %>% filter(type == "Action") %>% select(actionl2code = code, x1 = x, y1 = y), by = "actionl2code") %>%
          left_join(nodes_df %>% filter(type == "Threat") %>% select(threatl2code = code, x2 = x, y2 = y), by = "threatl2code") %>%
          mutate(hover_text = paste0("<b>Target:</b> ", target_name, " (", group_context, ")<br><b>Action:</b> ", actionl2code, " ", source_name, "<br><b>Threat:</b> ", threatl2code, " ", threat_name), safe_edge_id = paste0("edge_", row_number()))
      }
      
      # 8. Build the Plot
      p <- ggplot() +
        geom_text(data = bg_labels, aes(x=x, y=y, label=label, angle=angle), size=14, color="grey90", fontface="bold") +
        geom_segment(data=separators, aes(x=x_in, y=y_in, xend=x_out, yend=y_out), color="grey70", linewidth=0.5, linetype="dashed")
      
      if(nrow(edges_df) > 0) {
        # We use the 'hover_css' and 'tooltip' to define the interaction 
        # but rely on the CSS in girafe() to manage the visual thickness.
        p <- p + geom_curve_interactive(
          data = edges_df, 
          aes(x = x1, y = y1, xend = x2, yend = y2, 
              color = group_context, 
              linetype = target_type, 
              tooltip = hover_text, 
              data_id = safe_edge_id), 
          curvature = 0.1, 
          alpha = 0.6, 
          linewidth = 0.3
        )
      }
      
      p <- p + 
        geom_point_interactive(data = nodes_df, aes(x = x, y = y, tooltip = full_text, data_id = safe_id), size = 3, color = "#212529") +
        geom_text_interactive(data = nodes_df, aes(x = 1.06*cos(angle), y = 1.06*sin(angle), label = code, angle = rad_angle, hjust = hjust, tooltip = full_text), size = 3, fontface = "bold", color = "#212529") +
        geom_text(data=l1_labels, aes(x=r*cos(angle), y=r*sin(angle), label=stringr::str_wrap(l1, 25), angle=text_angle, hjust=hjust), size=2.8, color="#AA5F40", fontface="bold") +
        geom_path(data = arc_df, aes(x = x, y = y, group = group), color = "#07234c", linewidth = 1.2) +
        geom_text(data = l0_labels_df, aes(x = r_label*cos(raw_mid), y = r_label*sin(raw_mid), label = label, angle = text_angle, hjust = hjust), size = 3.5, color = "#0D67B8", fontface = "bold") +
        scale_linetype_manual(name = "Target Type", values = c("Species" = "solid", "Habitat" = "dashed")) +
        scale_color_viridis_d(name = "Target Group", option = "turbo") + 
        labs(caption = "💡 Click the magnifying glass (without the box) in the top right to enable click-and-drag panning.") +
        expand_limits(x = c(-3.6, 3.6), y = c(-2.8, 2.8)) + 
        theme_void() +
        theme(
          legend.position = "right", 
          legend.box = "vertical", 
          legend.title = element_text(face = "bold"), 
          plot.caption = element_text(color = "grey40", size = 11, face = "italic", hjust = 0.5),
          plot.margin = margin(10, 10, 10, 10)
        )
      
      # Build the final Girafe object
      girafe(ggobj = p, width_svg = 18, height_svg = 12,
             options = list(
               opts_sizing(rescale = TRUE, width = 1),
               opts_zoom(min = 1, max = 10),
               opts_toolbar(position = "topright", saveaspng = TRUE),
       
               opts_hover(css = "stroke:black;stroke-width:1.5px;cursor:pointer;stroke-opacity:1;opacity:1;"),
               opts_hover_inv(css = "opacity:0.8;"),
               opts_tooltip(css = "background-color:white;color:black;padding:10px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);")
             ))
    })
    # --- 4C. Absolute Domain Matrix (The Masterpiece) ---
    output$heatmap_plot <- renderPlotly({
      df <- links_data()
      
      # 1. Pull lexicon
      q_act <- "SELECT l0a.actionl0id, TRIM(COALESCE(l0a.actionl0name, 'Unknown')) AS l0, l1a.actionl1id, TRIM(COALESCE(l1a.actionl1name, 'Unknown')) AS l1, l2a.actionl2id, TRIM(COALESCE(l2a.actionl2code, 'Uncoded')) AS l2c, TRIM(COALESCE(l2a.actionl2name, 'Unknown')) AS l2n FROM proj.l2_actions l2a LEFT JOIN proj.l1_actions l1a ON l2a.actionl1id = l1a.actionl1id LEFT JOIN proj.l0_actions l0a ON l1a.actionl0id = l0a.actionl0id"
      q_thr <- "SELECT l0t.threatl0id, TRIM(COALESCE(l0t.threatl0name, 'Unknown')) AS l0, l1t.threatl1id, TRIM(COALESCE(l1t.threatl1name, 'Unknown')) AS l1, l2t.threatl2id, TRIM(COALESCE(l2t.threatl2code, 'Uncoded')) AS l2c, TRIM(COALESCE(l2t.threatl2name, 'Unknown')) AS l2n FROM proj.l2_threats l2t LEFT JOIN proj.l1_threats l1t ON l2t.threatl1id = l1t.threatl1id LEFT JOIN proj.l0_threats l0t ON l1t.threatl0id = l0t.threatl0id"
      
      # Force perfectly numerical sorting (8, 9, 10, 11)
      y_axis_df <- dbGetQuery(db, q_act) %>% 
        filter(!l0 %in% c("D. None", "Unknown", "None"), l2c != "11.1") %>% 
        mutate(
          major_code = suppressWarnings(as.numeric(sub("\\..*", "", l2c))),
          minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", l2c)))
        ) %>%
        arrange(l0, major_code, minor_code)
      
      x_axis_df <- dbGetQuery(db, q_thr) %>% 
        filter(!grepl("\\.9$", l2c), !l0 %in% c("Unknown Threats", "Unknown", "None"), threatl1id != 4) %>% 
        mutate(
          major_code = suppressWarnings(as.numeric(sub("\\..*", "", l2c))),
          minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", l2c)))
        ) %>%
        arrange(l0, major_code, minor_code)
      
      # Lock factor levels
      y_axis_df$l2c_factor <- factor(y_axis_df$l2c, levels = unique(y_axis_df$l2c))
      x_axis_df$l2c_factor <- factor(x_axis_df$l2c, levels = unique(x_axis_df$l2c))
      
      # 2. Compile Main Grid Counts
      actual_counts <- data.frame()
      if (nrow(df) > 0) {
        actual_counts <- df %>% group_by(actionl2id, threatl2id) %>% summarize(total_logs = n(), target_count = n_distinct(target_name), target_list = paste(sort(unique(target_name)), collapse = ", "), .groups = "drop")
      }
      
      z_main <- matrix(0, nrow = nrow(y_axis_df), ncol = nrow(x_axis_df))
      text_main <- matrix("", nrow = nrow(y_axis_df), ncol = nrow(x_axis_df))
      
      for (r in 1:nrow(y_axis_df)) {
        for (c in 1:nrow(x_axis_df)) {
          match_idx <- which(actual_counts$actionl2id == y_axis_df$actionl2id[r] & actual_counts$threatl2id == x_axis_df$threatl2id[c])
          if (length(match_idx) > 0) {
            val <- actual_counts$target_count[match_idx]
            logs <- actual_counts$total_logs[match_idx]
            t_list <- paste(strwrap(actual_counts$target_list[match_idx], width = 45), collapse = "<br>")
            z_main[r, c] <- val
            text_main[r, c] <- paste0("<b>Action:</b> ", y_axis_df$l2c[r], " - ", y_axis_df$l2n[r], "<br><br><b>Threat:</b> ", x_axis_df$l2c[c], " - ", x_axis_df$l2n[c], "<br><br><b>Total Actions/Threats Combo Loged:</b> ", logs, "<br><b>Distinct Species/Habitats:</b> ", val, "<br><b>Species/Habitats Addressed:</b><br><i>", t_list, "</i>")
          } else {
            z_main[r, c] <- 0
            text_main[r, c] <- paste0("<b>Action:</b> ", y_axis_df$l2c[r], " - ", y_axis_df$l2n[r], "<br><b>Threat:</b> ", x_axis_df$l2c[c], " - ", x_axis_df$l2n[c], "<br><br>No Species/Habitats Addressed")
          }
        }
      }
      
      # 3. Generate distinct color IDs
      y_axis_df$l0_z <- as.numeric(factor(y_axis_df$l0, levels = unique(y_axis_df$l0)))
      y_axis_df$l1_z <- as.numeric(factor(y_axis_df$l1, levels = unique(y_axis_df$l1))) + max(y_axis_df$l0_z)
      
      x_axis_df$l0_z <- as.numeric(factor(x_axis_df$l0, levels = unique(x_axis_df$l0))) + max(y_axis_df$l1_z)
      x_axis_df$l1_z <- as.numeric(factor(x_axis_df$l1, levels = unique(x_axis_df$l1))) + max(x_axis_df$l0_z)
      
      total_cats <- max(x_axis_df$l1_z)
      
      z_left <- cbind(y_axis_df$l0_z, y_axis_df$l1_z)
      z_bottom <- rbind(x_axis_df$l0_z, x_axis_df$l1_z)
      
      hover_left <- cbind(paste("Level 0 Action:", y_axis_df$l0), paste("Level 1 Action:", y_axis_df$l1))
      hover_bottom <- rbind(paste("Level 0 Threat:", x_axis_df$l0), paste("Level 1 Threat:", x_axis_df$l1))
      
      # 4. Custom Scales
      cpw_main_scale <- list(
        c(0, "white"),          
        c(0.001, "#9ecae1"),    
        c(1, "#07234c")         
      )
      
      base_colors <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
      full_palette <- colorRampPalette(base_colors)(total_cats)
      axis_colorscale <- list()
      for(i in 1:total_cats) { axis_colorscale[[i]] <- c((i-1)/(total_cats-1), full_palette[i]) }
      
      # 5. Text Annotations
      helper_wrap <- function(x, w) gsub("\n", "<br>", stringr::str_wrap(x, width = w))
      ann_list <- list()
      
      # THE FIX: Decoupled Main Axis Titles safely tucked within the expanded margins
      ann_list[[length(ann_list)+1]] <- list(xref="paper", yref="paper", x=0.56, y=-0.8, text="<b>Level 2 Threats</b>", showarrow=FALSE, font=list(size=14, color="black"))
      ann_list[[length(ann_list)+1]] <- list(xref="paper", yref="paper", x=-0.08, y=0.56, text="<b>Level 2 Actions</b>", textangle=-90, showarrow=FALSE, font=list(size=14, color="black"))
      
      l0_blocks <- y_axis_df %>% mutate(idx = row_number() - 1) %>% group_by(l0) %>% summarize(mid = mean(idx), .groups="drop")
      for(i in 1:nrow(l0_blocks)) { ann_list[[length(ann_list)+1]] <- list(xref="x2", yref="y2", x=0, y=l0_blocks$mid[i], text=paste0("<b>", helper_wrap(l0_blocks$l0[i], 12), "</b>"), textangle=-90, showarrow=FALSE, font=list(size=10, color="black")) }
      
      l1_blocks <- y_axis_df %>% mutate(idx = row_number() - 1) %>% group_by(l1) %>% summarize(mid = mean(idx), .groups="drop")
      for(i in 1:nrow(l1_blocks)) { ann_list[[length(ann_list)+1]] <- list(xref="x2", yref="y2", x=1, y=l1_blocks$mid[i], text=paste0("<b>", helper_wrap(l1_blocks$l1[i], 18), "</b>"), textangle=0, showarrow=FALSE, font=list(size=8, color="black")) }
      
      l0_t_blocks <- x_axis_df %>% mutate(idx = row_number() - 1) %>% group_by(l0) %>% summarize(mid = mean(idx), .groups="drop")
      for(i in 1:nrow(l0_t_blocks)) { ann_list[[length(ann_list)+1]] <- list(xref="x3", yref="y3", x=l0_t_blocks$mid[i], y=0, text=paste0("<b>", l0_t_blocks$l0[i], "</b>"), textangle=0, showarrow=FALSE, font=list(size=10, color="black")) }
      
      l1_t_blocks <- x_axis_df %>% mutate(idx = row_number() - 1) %>% group_by(l1) %>% summarize(mid = mean(idx), .groups="drop")
      for(i in 1:nrow(l1_t_blocks)) { ann_list[[length(ann_list)+1]] <- list(xref="x3", yref="y3", x=l1_t_blocks$mid[i], y=1, text=paste0("<b>", helper_wrap(l1_t_blocks$l1[i], 18), "</b>"), textangle=0, showarrow=FALSE, font=list(size=8, color="black")) }
      
      # 6. Build the Locked Traces
      plot_ly() %>%
        add_trace(
          type = "heatmap", x = x_axis_df$l2c_factor, y = y_axis_df$l2c_factor, z = z_main, text = text_main,
          hoverinfo = "text", colorscale = cpw_main_scale, xaxis = "x", yaxis = "y",
          xgap = 1, ygap = 1,
          colorbar = list(title="<b>Distinct<br>Species/Habitats</b>", x=1.02, y=0.5, yanchor="middle", len=0.6, dtick=1) 
        ) %>%
        add_trace(
          type = "heatmap", x = c("L0 Action", "L1 Action"), y = y_axis_df$l2c_factor, z = z_left, text = hover_left,
          hoverinfo = "text", zmin=1, zmax=total_cats, colorscale = axis_colorscale, showscale = FALSE, xaxis = "x2", yaxis = "y2",
          xgap = 0, ygap = 0 
        ) %>%
        add_trace(
          type = "heatmap", x = x_axis_df$l2c_factor, y = c("L0 Threat", "L1 Threat"), z = z_bottom, text = hover_bottom,
          hoverinfo = "text", zmin=1, zmax=total_cats, colorscale = axis_colorscale, showscale = FALSE, xaxis = "x3", yaxis = "y3",
          xgap = 0, ygap = 0 
        ) %>%
        layout(
          plot_bgcolor = "black",   
          paper_bgcolor = "white",  
          annotations = ann_list,
          
          # White masks dropping beneath the axes
          shapes = list(
            list(type = "rect", x0 = 0.07, x1 = 0.11, xref = "paper", y0 = 0, y1 = 1, yref = "paper", fillcolor = "white", line = list(color = "white"), layer = "below"),
            list(type = "rect", x0 = 0, x1 = 1, xref = "paper", y0 = 0.07, y1 = 0.11, yref = "paper", fillcolor = "white", line = list(color = "white"), layer = "below"),
            list(type = "rect", x0 = 0, x1 = 0.11, xref = "paper", y0 = 0, y1 = 0.11, yref = "paper", fillcolor = "white", line = list(color = "white"), layer = "below")
          ),
          
          # THE FIX: `range` explicitly prevents Plotly from adding 5% padding. The Black Borders are dead!
          xaxis  = list(domain = c(0.11, 1.0), type = "category", side = "bottom", showticklabels = TRUE, tickangle = 0, tickfont = list(color="black", size=11, face="bold"), fixedrange = TRUE, title = "", range = c(-0.5, nrow(x_axis_df) - 0.5)),
          yaxis  = list(domain = c(0.11, 1.0), type = "category", side = "left", showticklabels = TRUE, tickfont = list(color="black", size=11, face="bold"), fixedrange = TRUE, title = "", range = c(-0.5, nrow(y_axis_df) - 0.5)),
          
          # Left Blocks (Range restricted from -0.5 to 1.5 because there are 2 columns)
          xaxis2 = list(domain = c(0.0, 0.07), type = "category", showticklabels = FALSE, fixedrange = TRUE, range = c(-0.5, 1.5)),
          yaxis2 = list(domain = c(0.11, 1.0), type = "category", side = "left", showticklabels = FALSE, matches = "y", fixedrange = TRUE),
          
          # Bottom Blocks (Range restricted from -0.5 to 1.5 because there are 2 rows)
          xaxis3 = list(domain = c(0.11, 1.0), type = "category", showticklabels = FALSE, matches = "x", fixedrange = TRUE),
          yaxis3 = list(domain = c(0.0, 0.07), type = "category", showticklabels = FALSE, fixedrange = TRUE, range = c(-0.5, 1.5)),
          
          # Increased Left Margin for the Actions Label
          margin = list(l = 150, r = 20, b = 120, t = 40)
        ) %>% config(displayModeBar = FALSE)
    })
    
  }) 
}