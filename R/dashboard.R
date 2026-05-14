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
library(leaflet)
library(sf)

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
      
      # ==================================================
      # TAB 1: EXPLORE BY Species/Habitat
      # ==================================================
      nav_panel("Explore by Species/Habitat", 
                fluidRow(class="mt-3",
                         column(width = 4,
                                div(class = "card shadow-sm mb-3",
                                    div(class = "card-header text-white", style = "background-color: #055A53;", "Select Species/Habitat"),
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
                                      div(class = "card-body text-center text-muted", style = "padding: 120px 20px;", h4("Select an action on the left to view its workspace."))
                                  )
                                ),
                                conditionalPanel(
                                  condition = sprintf("input['%s'] != null", ns("target_actions_table_rows_selected")),
                                  navset_underline(
                                    id = ns("targ_nav_tabs"),
                                    nav_panel("Overview", value = "overview",
                                              div(class="mt-3", layout_columns(
                                                div(
                                                  div(class = "card shadow-sm mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #0D67B8;", "Description"), div(class = "card-body", uiOutput(ns("targ_desc_ui")))),
                                                  div(class = "card shadow-sm mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Other Species/Habitats targeted by this Action"), div(class = "card-body", uiOutput(ns("targ_other_targets_ui"))))
                                                ),
                                                div(class = "card shadow-sm mb-3", div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"), div(class = "card-body", uiOutput(ns("targ_threats_ui"))))
                                              ))
                                    ),
                                    nav_panel("Progress Logs", value = "logs",
                                              div(class="mt-3", div(class = "card shadow-sm mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #07234C;", "Results Chain"), div(class = "card-body", uiOutput(ns("targ_results_chain_ui")))),
                                                  div(class = "card shadow-sm", div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "History"), div(class = "card-body", uiOutput(ns("targ_updates_list")))))
                                    ),
                                    nav_panel("Spatial Footprint", value = "spatial",
                                              div(class="mt-3 card shadow-sm", div(class="card-body",
                                                                                   layout_columns(
                                                                                     div(class="p-2 text-center rounded border", style="background-color: #F8F9FA;", h6("Scale Category", class="text-muted mb-0"), h5(textOutput(ns("targ_map_scale")), class="mb-0 fw-bold", style="color:#0D67B8;")),
                                                                                     div(class="p-2 text-center rounded border", style="background-color: #F8F9FA;", h6("Approx. Area", class="text-muted mb-0"), h5(textOutput(ns("targ_map_area")), class="mb-0 fw-bold", style="color:#055A53;"))
                                                                                   ),
                                                                                   div(class="mt-3 border rounded", leafletOutput(ns("targ_map"), height="500px"))
                                              )))
                                  )
                                )
                         )
                )
      ),
      
      # ==================================================
      # TAB 2: EXPLORE BY ACTION
      # ==================================================
      nav_panel("Explore by Action",
                fluidRow(class="mt-3",
                         column(width = 4,
                                div(class = "card shadow-sm mb-3",
                                    div(class = "card-header text-white", style = "background-color: #055A53;", "Select Action"),
                                    div(class = "card-body",
                                        selectizeInput(ns("dash_l0"), "Level 0 Category", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                                        selectizeInput(ns("dash_l1"), "Level 1 Category", choices = c("Select Level 0 first..." = ""),options = list(dropdownParent = "body")),
                                        selectizeInput(ns("dash_l2"), "Level 2 Action", choices = c("Select Level 1 first..." = ""),options = list(dropdownParent = "body")),
                                        hr(class = "my-4"),
                                        DTOutput(ns("action_targets_table"))
                                    )
                                )
                         ),
                         column(width = 8,
                                conditionalPanel(
                                  condition = sprintf("input['%s'] == null", ns("action_targets_table_rows_selected")),
                                  div(class = "card shadow-sm", style = "background-color: #FAFAFA; border: 2px dashed #CCCCCC;",
                                      div(class = "card-body text-center text-muted", style = "padding: 120px 20px;", h4("Select a target on the left to view its workspace."))
                                  )
                                ),
                                conditionalPanel(
                                  condition = sprintf("input['%s'] != null", ns("action_targets_table_rows_selected")),
                                  navset_underline(
                                    id = ns("act_nav_tabs"),
                                    nav_panel("Overview", value = "overview",
                                              div(class="mt-3", layout_columns(
                                                div(
                                                  div(class = "card shadow-sm mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #0D67B8;", "Description"), div(class = "card-body", uiOutput(ns("act_desc_ui")))),
                                                  div(class = "card shadow-sm mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "Species/Habitats targeted by this Action"), div(class = "card-body", uiOutput(ns("act_other_targets_ui"))))
                                                ),
                                                div(class = "card shadow-sm mb-3", div(class = "card-header bg-secondary text-dark fw-bold", "Mitigated Threats"), div(class = "card-body", uiOutput(ns("act_threats_ui"))))
                                              ))
                                    ),
                                    nav_panel("Progress Logs", value = "logs",
                                              div(class="mt-3", div(class = "card shadow-sm mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #07234C;", "Results Chain"), div(class = "card-body", uiOutput(ns("act_results_chain_ui")))),
                                                  div(class = "card shadow-sm", div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "History"), div(class = "card-body", uiOutput(ns("act_updates_list")))))
                                    ),
                                    nav_panel("Spatial Footprint", value = "spatial",
                                              div(class="mt-3 card shadow-sm", div(class="card-body",
                                                                                   layout_columns(
                                                                                     div(class="p-2 text-center rounded border", style="background-color: #F8F9FA;", h6("Scale Category", class="text-muted mb-0"), h5(textOutput(ns("act_map_scale")), class="mb-0 fw-bold", style="color:#0D67B8;")),
                                                                                     div(class="p-2 text-center rounded border", style="background-color: #F8F9FA;", h6("Approx. Area", class="text-muted mb-0"), h5(textOutput(ns("act_map_area")), class="mb-0 fw-bold", style="color:#055A53;"))
                                                                                   ),
                                                                                   div(class="mt-3 border rounded", leafletOutput(ns("act_map"), height="500px"))
                                              )))
                                  )
                                )
                         )
                )
      ),
      
      # ==================================================
      # TAB 3: ALL ACTIONS 
      # ==================================================
      nav_panel("All Actions List",
                fluidRow(class="mt-3",
                         column(width = 12,
                                div(class = "card shadow-sm mb-3 border-0",
                                    div(class = "card-header text-white", style = "background-color: #055A53;", "Tracked Actions"),
                                    div(class = "card-body",
                                        p(class="text-muted", "Click on any row to open the full Read-Only profile for that action."),
                                        DTOutput(ns("all_actions_table"))
                                    )
                                )
                         )
                )
      ),
      
      # ==================================================
      # TAB 4: VISUALIZATIONS (REFACTORED)
      # ==================================================
      nav_panel("Explore Action/Threat Connections",
                div(class = "mt-3",
                    layout_columns(
                      div(class = "card shadow-sm mb-3",
                          div(class = "card-body p-3",
                              selectInput(ns("conn_filter_type"), "Filter Network By Target Type:", choices = c("All", "Species Only", "Habitats Only"), width = "100%")
                          )
                      ),
                      div(class = "d-flex align-items-center h-100", p(em("Visualize exactly how Conservation Actions flow through specific Species/Habitats to mitigate Threats.", class="text-muted small mb-0")))
                    ),
                    
                    navset_underline(
                      id = ns("conn_nav_tabs"),
                      
                      nav_panel("Sankey Chart", value = "sankey", 
                                div(class = "card shadow-sm mt-3 border-0", 
                                    div(class = "card-body", style = "min-height: 750px; overflow: hidden;", 
                                        plotlyOutput(ns("sankey_plot"), height = "700px", width = "100%")
                                    )
                                )
                      ),
                      
                      nav_panel("Chord Graph", value = "bipartite", 
                                div(class = "card shadow-sm mt-3 border-0", 
                                    div(class = "card-body", style = "min-height: 750px; overflow: hidden;", 
                                        girafeOutput(ns("bipartite_plot"), width = "100%", height = "750px")
                                    )
                                )
                      ),
                      
                      nav_panel("Heatmap", value = "heatmap", 
                                div(class = "card shadow-sm mt-3 border-0", 
                                    div(class = "card-body", style = "min-height: 750px; overflow: hidden;", 
                                        plotlyOutput(ns("heatmap_plot"), height = "700px", width = "100%")
                                    )
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
    ns <- session$ns
    
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
    
    # HELPER: Area Formatting
    format_acres <- function(acres) {
      if(is.na(acres) || is.null(acres)) return("N/A")
      paste0(prettyNum(round(acres, 1), big.mark = ","), " Acres")
    }
    
    # HELPER: Draw Statewide Polygon
    draw_statewide_map <- function(m) {
      co_geom <- sf::st_polygon(list(matrix(c(-109.05, 37.0, -102.04, 37.0, -102.04, 41.0, -109.05, 41.0, -109.05, 37.0), ncol=2, byrow=TRUE)))
      co_sf <- sf::st_sfc(co_geom, crs = 4326) %>% sf::st_sf()
      m %>% addPolygons(data = co_sf, color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.2, weight = 2)
    }
    
    # -----------------------------------------------------
    # TAB 1: EXPLORE BY SPECIES/HABITAT
    # -----------------------------------------------------
    targ_actions_data <- reactive({
      db_sync_trigger(); is_sp <- input$targ_type == "Species"; tid <- if(is_sp) input$dash_species else input$dash_habitat
      if (is.null(tid) || tid == "") return(data.frame())
      
      q <- if(is_sp) {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", COALESCE(sadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.timeframe AS \"Timeframe\", ia.implementation_progress AS \"Impl. Progress\", ia.actiondesc
         FROM track.implementedactions ia 
         JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid 
         JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id 
         LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid 
         WHERE sha.speciesid = $1"
      } else {
        "SELECT ia.implementedactionid, sha.specieshabitatactionsid, l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", COALESCE(hadd.\"Meaningful.Details\", 'None') AS \"Action Detail\", ia.timeframe AS \"Timeframe\", ia.implementation_progress AS \"Impl. Progress\", ia.actiondesc
         FROM track.implementedactions ia 
         JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid 
         JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id 
         LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid 
         WHERE sha.habitatsubtypeid = $1"
      }
      dbGetQuery(db, q, params = list(as.integer(tid)))
    })
    
    output$target_actions_table <- renderDT({
      df <- targ_actions_data(); 
      if(nrow(df) == 0) return(datatable(data.frame(Message = "No species/habitats selected."), rownames = F, options = list(dom = 't')))
      datatable(df, selection = "single", rownames = F, options = list(dom = 't', paging= FALSE, scrollY="250px", scrollCollapse=TRUE, columnDefs = list(list(visible = F, targets = c(0, 1, 3, 6)))))
    })
    
    # THE FIX: Added length() and is.null() checks to prevent logical(0) crashes
    output$targ_desc_ui <- renderUI({
      req(input$target_actions_table_rows_selected)
      row <- targ_actions_data()[input$target_actions_table_rows_selected, ]
      desc <- if(length(row$actiondesc) == 0 || is.na(row$actiondesc) || trimws(row$actiondesc) == "") "No description provided." else row$actiondesc
      detail <- if(length(row$`Action Detail`) == 0 || is.na(row$`Action Detail`) || trimws(row$`Action Detail`) == "") "None" else row$`Action Detail`
      tagList(p(class = "mb-2", strong("Impl. Progress: "), row$`Impl. Progress`, span(style="margin: 0 10px;", "|"), strong("Timeframe: "), row$Timeframe), p(class = "mb-2", strong("Description: "), br(), em(desc)), p(class = "mb-0", strong("Detail: "), br(), em(detail)))
    })
    
    output$targ_threats_ui <- renderUI({
      req(input$target_actions_table_rows_selected); sha_id <- targ_actions_data()[input$target_actions_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification, ta.alternative_category, ta.justification_text FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.specieshabitatactionsid = $1"
      threats <- dbGetQuery(db, q, params = list(as.integer(sha_id)))
      if(nrow(threats) == 0) return(p("No threats recorded."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(threats), function(i) {
        if (threats$threatl2id[i] == 59) { tags$li(strong("Broader Conservation Goal:", style = "color: #0D67B8;"), span(style = "font-weight: bold; margin-left: 5px;", threats$alternative_category[i]), br(), em(ifelse(is.na(threats$justification_text[i]) | trimws(threats$justification_text[i])=="", "No details provided.", threats$justification_text[i])), class="mb-3")
        } else { tags$li(strong(threats$t_name[i]), br(), em(ifelse(is.na(threats$justification[i]) | trimws(threats$justification[i])=="", "No justification provided.", threats$justification[i])), class="mb-3") }
      }))
    })
    
    output$targ_other_targets_ui <- renderUI({
      req(input$target_actions_table_rows_selected); impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]; sha_id <- targ_actions_data()[input$target_actions_table_rows_selected, "specieshabitatactionsid"]
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type, COALESCE(sadd.\"Meaningful.Details\", hadd.\"Meaningful.Details\", 'None') AS detail_text FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE sha.implementedactionid = $1 AND sha.specieshabitatactionsid != $2 ORDER BY target_type DESC, target_name ASC"
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id), as.integer(sha_id)))
      if(nrow(others) == 0) return(p("No other targets for this action."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(others), function(i) { tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")", br(), em("Detail: ", others$detail_text[i]), class="mb-2") }))
    })
    
    targ_updates_raw <- reactive({
      req(input$target_actions_table_rows_selected); impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT TO_CHAR(a.actiondate, 'MM/DD/YYYY') AS \"Date\", a.implementation_progress AS \"Impl. Progress\", a.result_progress AS \"Effectiveness\", COALESCE(ip.color_hex, '#FFFFFF') AS ip_color, COALESCE(rp.color_hex, '#FFFFFF') AS rp_color, a.what_done AS \"What was done?\", a.what_learned AS \"What was learned?\", a.what_needed AS \"What is needed?\" FROM track.actiontracking a LEFT JOIN lkup.implementation_progress ip ON a.implementation_progress = ip.progress_name LEFT JOIN lkup.result_progress rp ON a.result_progress = rp.result_name WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC"
      dbGetQuery(db, q, params = list(as.integer(impl_id)))
    })
    
    output$targ_updates_list <- renderUI({
      df <- targ_updates_raw()
      if(nrow(df) == 0) return(p(class="text-muted text-center mt-3", em("No progress logs recorded yet.")))
      div(style = "max-height: 400px; overflow-y: auto; padding-right: 10px;", lapply(1:nrow(df), function(i) {
        ip_text_col <- if(df$ip_color[i] %in% c("#FF4040", "#228B22")) "white" else "black"; rp_text_col <- if(df$rp_color[i] %in% c("#FF4040", "#228B22")) "white" else "black"
        ip_badge <- span(class = "badge", style = paste0("background-color: ", df$ip_color[i], "; color: ", ip_text_col, "; font-size: 0.85em; border: 1px solid #ccc;"), df$`Impl. Progress`[i])
        rp_badge <- span(class = "badge", style = paste0("background-color: ", df$rp_color[i], "; color: ", rp_text_col, "; font-size: 0.85em; border: 1px solid #ccc;"), df$Effectiveness[i])
        div(class = "mb-3 p-3 rounded shadow-sm", style = "background-color: #F8F9FA; border: 1px solid #DEE2E6;",
            div(class = "d-flex justify-content-between align-items-center mb-3", style="border-bottom: 2px solid #e9ecef; padding-bottom: 8px;", strong(df$Date[i], style="font-size: 1.1em; color: #07234C;"), div(style = "display: flex; align-items: center; gap: 5px;", span(style="font-size: 0.85em; font-weight: bold; color: #6c757d;", "Implementation:"), ip_badge, span(style="margin-left: 10px; font-size: 0.85em; font-weight: bold; color: #6c757d;", "Effectiveness:"), rp_badge)),
            if(!is.na(df$`What was done?`[i]) && df$`What was done?`[i] != "") div(class="mb-3", strong("What was done?", style="color: #055A53;"), p(df$`What was done?`[i], class="text-dark mt-1 mb-0", style="font-size: 0.95em;")) else NULL,
            if(!is.na(df$`What was learned?`[i]) && df$`What was learned?`[i] != "") div(class="mb-3", strong("What was learned?", style="color: #055A53;"), p(df$`What was learned?`[i], class="text-dark mt-1 mb-0", style="font-size: 0.95em;")) else NULL,
            if(!is.na(df$`What is needed?`[i]) && df$`What is needed?`[i] != "") div(class="mb-1", strong("What is needed?", style="color: #055A53;"), p(df$`What is needed?`[i], class="text-dark mt-1 mb-0", style="font-size: 0.95em;")) else NULL)
      }))
    })
    
    output$targ_results_chain_ui <- renderUI({
      req(input$target_actions_table_rows_selected); row <- targ_actions_data()[input$target_actions_table_rows_selected, ]; impl_id <- row$implementedactionid; sha_id <- row$specieshabitatactionsid
      q_colors <- "SELECT ia.implementation_progress, ia.result_progress, ip.color_hex AS impl_color, rp.color_hex AS res_color FROM track.implementedactions ia LEFT JOIN lkup.implementation_progress ip ON ia.implementation_progress = ip.progress_name LEFT JOIN lkup.result_progress rp ON ia.result_progress = rp.result_name WHERE ia.implementedactionid = $1"
      colors_df <- dbGetQuery(db, q_colors, params = list(as.integer(impl_id)))
      impl_col <- if(nrow(colors_df) > 0 && !is.na(colors_df$impl_color)) colors_df$impl_color else "#FFFFFF"; res_col <- if(nrow(colors_df) > 0 && !is.na(colors_df$res_color)) colors_df$res_color else "#FFFFFF"
      impl_text <- if(nrow(colors_df) > 0) colors_df$implementation_progress else "Unknown"; res_text <- if(nrow(colors_df) > 0) colors_df$result_progress else "Unknown"
      is_sp <- input$targ_type == "Species"; target_label <- if(is_sp) input$dash_species else input$dash_habitat
      q_targ_name <- if(is_sp) "SELECT commonname AS name FROM proj.species WHERE speciesid = $1" else "SELECT habitatsubtypename AS name FROM proj.habitatsubtypes WHERE habitatsubtypeid = $1"
      t_name_df <- dbGetQuery(db, q_targ_name, params = list(as.integer(target_label))); target_name <- if(nrow(t_name_df) > 0) t_name_df$name[1] else "Selected Target"
      q_threat_names <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS t_name FROM track.threatsaddressed ta JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE ta.specieshabitatactionsid = $1 AND ta.threatl2id != 59 UNION SELECT alternative_category AS t_name FROM track.threatsaddressed WHERE specieshabitatactionsid = $1 AND threatl2id = 59"
      t_names <- dbGetQuery(db, q_threat_names, params = list(as.integer(sha_id)))
      threat_label <- if(nrow(t_names) == 1) t_names$t_name[1] else if(nrow(t_names) > 1) paste0(nrow(t_names), " Threats/Goals") else "No Threats Mapped"
      div(style = "display: flex; align-items: stretch; justify-content: space-between; gap: 10px; text-align: center; margin-top: 10px; margin-bottom: 10px;",
          div(style = paste0("flex: 1; border-radius: 8px; padding: 15px; border: 2px solid #ccc; background-color:", impl_col, ";"), h6("Action / Strategy", style = "font-size: 0.85em; text-transform: uppercase; color: #333; margin-bottom: 5px;"), strong(row$Action, style = "display: block; font-size: 1.0em; color: #000;"), hr(style = "margin: 8px 0; border-top: 1px solid #666;"), span(style = "font-size: 0.9em; font-weight: bold; color: #333;", paste("Impl:", impl_text))),
          div(style = "display: flex; align-items: center; color: #999; font-size: 1.5em;", icon("arrow-right")),
          div(style = paste0("flex: 1; border-radius: 8px; padding: 15px; border: 2px solid #ccc; background-color:", res_col, ";"), h6("Mitigated Threat / Goal", style = "font-size: 0.85em; text-transform: uppercase; color: #333; margin-bottom: 5px;"), strong(threat_label, style = "display: block; font-size: 1.0em; color: #000; word-wrap: break-word;"), hr(style = "margin: 8px 0; border-top: 1px solid #666;"), span(style = "font-size: 0.9em; font-weight: bold; color: #333;", paste("Result:", res_text))),
          div(style = "display: flex; align-items: center; color: #999; font-size: 1.5em;", icon("arrow-right")),
          div(style = "flex: 1; border-radius: 8px; padding: 15px; border: 2px solid #ccc; background-color: #E2E8F0;", h6("Conservation Target", style = "font-size: 0.85em; text-transform: uppercase; color: #333; margin-bottom: 5px;"), strong(target_name, style = "display: block; font-size: 1.0em; color: #000; word-wrap: break-word;"))
      )
    })
    
    targ_spatial_data <- reactive({
      req(input$target_actions_table_rows_selected)
      impl_id <- targ_actions_data()[input$target_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT scale_category, ST_AsGeoJSON(geom) as geojson, ST_Area(geom::geography) * 0.000247105 AS acres FROM track.action_spatial WHERE implementedactionid = $1"
      dbGetQuery(db, q, params = list(as.integer(impl_id)))
    })
    
    output$targ_map_scale <- renderText({
      df <- targ_spatial_data()
      if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
    })
    
    output$targ_map_area <- renderText({
      df <- targ_spatial_data()
      scale <- if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
      if (scale == "Statewide") return("~66,600,000 Acres (Statewide)")
      if(nrow(df) > 0 && !is.na(df$acres)) format_acres(df$acres) else "N/A"
    })
    
    output$targ_map <- renderLeaflet({
      req(input$targ_nav_tabs == "spatial")
      df <- targ_spatial_data()
      scale <- if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
      
      m <- leaflet() %>% setView(lng = -105.5, lat = 39.0, zoom = 6) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addLayersControl(baseGroups = c("Terrain", "Satellite"), options = layersControlOptions(collapsed = FALSE))
      
      if (scale == "Statewide") {
        m <- draw_statewide_map(m)
      } else if(nrow(df) > 0 && !is.na(df$geojson)) {
        tryCatch({
          geo_sf <- sf::st_as_sfc(df$geojson, GeoJSON = TRUE) %>% sf::st_sf(crs = 4326) %>% sf::st_make_valid()
          if(nrow(geo_sf) > 0) m <- m %>% addPolygons(data = geo_sf, color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 2)
        }, error = function(e) { print(e) })
      }
      m
    })
    
    
    # -----------------------------------------------------
    # TAB 2 LOGIC (EXPLORE BY ACTION)
    # -----------------------------------------------------
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
      q <- "SELECT ia.implementedactionid, l2.actionl2code || '. ' || l2.actionl2name AS \"Action\", ia.implementation_progress AS \"Impl. Progress\", ia.timeframe AS \"Timeframe\", ia.actiondesc, STRING_AGG(CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END, ', ') AS \"Included Targets\" FROM track.implementedactions ia JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE ia.actionl2id = $1 GROUP BY ia.implementedactionid, l2.actionl2code, l2.actionl2name, ia.implementation_progress, ia.timeframe, ia.actiondesc ORDER BY ia.implementedactionid DESC"
      dbGetQuery(db, q, params = list(as.integer(input$dash_l2)))
    })
    
    output$action_targets_table <- renderDT({
      df <- act_targets_data()
      if(nrow(df) == 0) return(datatable(data.frame(Message = "No action selected."), rownames = F, options = list(dom = 't')))
      datatable(df, selection = "single", rownames = F, options = list(dom = 'ft', paging = FALSE, scrollY = "250px", scrollCollapse = TRUE, info = FALSE, columnDefs = list(list(visible = F, targets = c(0, 4)))))
    })
    
    # THE FIX: Added length() and is.null() checks to prevent logical(0) crashes
    output$act_desc_ui <- renderUI({
      req(input$action_targets_table_rows_selected); row <- act_targets_data()[input$action_targets_table_rows_selected, ]
      desc <- if(length(row$actiondesc) == 0 || is.na(row$actiondesc) || trimws(row$actiondesc) == "") "No description provided." else row$actiondesc
      tagList(p(class = "mb-2", strong("Impl. Progress: "), row$`Impl. Progress`, span(style="margin: 0 10px;", "|"), strong("Timeframe: "), row$Timeframe), p(class = "mb-0", strong("Description: "), br(), em(desc)))
    })
    
    # THE FIX: Changed 'threats_df$threat_name' to 'threats_df$t_name' so it correctly references the SQL column alias.
    output$act_threats_ui <- renderUI({
      req(input$action_targets_table_rows_selected); impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS t_name, ta.justification, ta.alternative_category, ta.justification_text, STRING_AGG(CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END, ', ') AS target_labels FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 GROUP BY l2.threatl2id, l2.threatl2code, l2.threatl2name, ta.justification, ta.alternative_category, ta.justification_text ORDER BY t_name ASC"
      threats_df <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      
      if(nrow(threats_df) > 0) {
        threats_df$group_name <- ifelse(threats_df$threatl2id == 59, paste0("Broader Goal: ", threats_df$alternative_category), threats_df$t_name)
        tags$ul(class = "mt-2 mb-0", lapply(unique(threats_df$group_name), function(g) {
          sub_df <- threats_df[threats_df$group_name == g, ]; is_broader <- sub_df$threatl2id[1] == 59
          title <- if(is_broader) strong("Broader Goal: ", style="color: #0D67B8;", sub_df$alternative_category[1]) else strong(sub_df$t_name[1])
          t_list <- tags$ul(class="mt-1 mb-3", style="list-style-type:circle;", lapply(1:nrow(sub_df), function(i) tags$li(span(class="text-primary fw-bold", sub_df$target_labels[i]), " - ", em(if(is_broader) sub_df$justification_text[i] else sub_df$justification[i]))))
          tags$li(class="mb-2", title, t_list)
        }))
      } else { p(em("No threats recorded.")) }
    })
    
    output$act_other_targets_ui <- renderUI({
      req(input$action_targets_table_rows_selected); impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS target_type, COALESCE(sadd.\"Meaningful.Details\", hadd.\"Meaningful.Details\", 'None') AS detail_text FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid LEFT JOIN proj.speciesactionsdetailsdistinct sadd ON sha.speciesactiondetailid = sadd.speciesactionsdetailsdistinctid LEFT JOIN proj.habitatactionsdetailsdistinct hadd ON sha.habitatactiondetailid = hadd.habitatactionsdetailsdistinctid WHERE sha.implementedactionid = $1 ORDER BY target_type DESC, target_name ASC"
      others <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(others) == 0) return(p("No species/habitats assigned."))
      tags$ul(class = "ps-3 mb-0", lapply(1:nrow(others), function(i) { tags$li(strong(others$target_name[i]), " (", others$target_type[i], ")", br(), em("Detail: ", others$detail_text[i]), class="mb-2") }))
    })
    
    act_updates_raw <- reactive({
      req(input$action_targets_table_rows_selected); impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT TO_CHAR(a.actiondate, 'MM/DD/YYYY') AS \"Date\", a.implementation_progress AS \"Impl. Progress\", a.result_progress AS \"Effectiveness\", COALESCE(ip.color_hex, '#FFFFFF') AS ip_color, COALESCE(rp.color_hex, '#FFFFFF') AS rp_color, a.what_done AS \"What was done?\", a.what_learned AS \"What was learned?\", a.what_needed AS \"What is needed?\" FROM track.actiontracking a LEFT JOIN lkup.implementation_progress ip ON a.implementation_progress = ip.progress_name LEFT JOIN lkup.result_progress rp ON a.result_progress = rp.result_name WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC"
      dbGetQuery(db, q, params = list(as.integer(impl_id)))
    })
    
    output$act_updates_list <- renderUI({
      df <- act_updates_raw()
      if(nrow(df) == 0) return(p(class="text-muted text-center mt-3", em("No progress logs recorded yet.")))
      div(style = "max-height: 400px; overflow-y: auto; padding-right: 10px;", lapply(1:nrow(df), function(i) {
        ip_text_col <- if(df$ip_color[i] %in% c("#FF4040", "#228B22")) "white" else "black"; rp_text_col <- if(df$rp_color[i] %in% c("#FF4040", "#228B22")) "white" else "black"
        ip_badge <- span(class = "badge", style = paste0("background-color: ", df$ip_color[i], "; color: ", ip_text_col, "; font-size: 0.85em; border: 1px solid #ccc;"), df$`Impl. Progress`[i])
        rp_badge <- span(class = "badge", style = paste0("background-color: ", df$rp_color[i], "; color: ", rp_text_col, "; font-size: 0.85em; border: 1px solid #ccc;"), df$Effectiveness[i])
        div(class = "mb-3 p-3 rounded shadow-sm", style = "background-color: #F8F9FA; border: 1px solid #DEE2E6;",
            div(class = "d-flex justify-content-between align-items-center mb-3", style="border-bottom: 2px solid #e9ecef; padding-bottom: 8px;", strong(df$Date[i], style="font-size: 1.1em; color: #07234C;"), div(style = "display: flex; align-items: center; gap: 5px;", span(style="font-size: 0.85em; font-weight: bold; color: #6c757d;", "Implementation:"), ip_badge, span(style="margin-left: 10px; font-size: 0.85em; font-weight: bold; color: #6c757d;", "Effectiveness:"), rp_badge)),
            if(!is.na(df$`What was done?`[i]) && df$`What was done?`[i] != "") div(class="mb-3", strong("What was done?", style="color: #055A53;"), p(df$`What was done?`[i], class="text-dark mt-1 mb-0", style="font-size: 0.95em;")) else NULL,
            if(!is.na(df$`What was learned?`[i]) && df$`What was learned?`[i] != "") div(class="mb-3", strong("What was learned?", style="color: #055A53;"), p(df$`What was learned?`[i], class="text-dark mt-1 mb-0", style="font-size: 0.95em;")) else NULL,
            if(!is.na(df$`What is needed?`[i]) && df$`What is needed?`[i] != "") div(class="mb-1", strong("What is needed?", style="color: #055A53;"), p(df$`What is needed?`[i], class="text-dark mt-1 mb-0", style="font-size: 0.95em;")) else NULL)
      }))
    })
    
    output$act_results_chain_ui <- renderUI({
      req(input$action_targets_table_rows_selected); row <- act_targets_data()[input$action_targets_table_rows_selected, ]; impl_id <- row$implementedactionid
      q_colors <- "SELECT ia.implementation_progress, ia.result_progress, ip.color_hex AS impl_color, rp.color_hex AS res_color FROM track.implementedactions ia LEFT JOIN lkup.implementation_progress ip ON ia.implementation_progress = ip.progress_name LEFT JOIN lkup.result_progress rp ON ia.result_progress = rp.result_name WHERE ia.implementedactionid = $1"
      colors_df <- dbGetQuery(db, q_colors, params = list(as.integer(impl_id)))
      impl_col <- if(nrow(colors_df) > 0 && !is.na(colors_df$impl_color)) colors_df$impl_color else "#FFFFFF"; res_col <- if(nrow(colors_df) > 0 && !is.na(colors_df$res_color)) colors_df$res_color else "#FFFFFF"
      impl_text <- if(nrow(colors_df) > 0) colors_df$implementation_progress else "Unknown"; res_text <- if(nrow(colors_df) > 0) colors_df$result_progress else "Unknown"
      q_threat_names <- "SELECT DISTINCT l2.threatl2code || '. ' || l2.threatl2name AS t_name FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id WHERE sha.implementedactionid = $1 AND ta.threatl2id != 59 UNION SELECT DISTINCT alternative_category AS t_name FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid WHERE sha.implementedactionid = $1 AND ta.threatl2id = 59"
      t_names <- dbGetQuery(db, q_threat_names, params = list(as.integer(impl_id)))
      threat_label <- if(nrow(t_names) == 1) t_names$t_name[1] else if(nrow(t_names) > 1) paste0(nrow(t_names), " Threats/Goals") else "No Threats Mapped"
      div(style = "display: flex; align-items: stretch; justify-content: space-between; gap: 10px; text-align: center; margin-top: 10px; margin-bottom: 10px;",
          div(style = paste0("flex: 1; border-radius: 8px; padding: 15px; border: 2px solid #ccc; background-color:", impl_col, ";"), h6("Action / Strategy", style = "font-size: 0.85em; text-transform: uppercase; color: #333; margin-bottom: 5px;"), strong(row$Action, style = "display: block; font-size: 1.0em; color: #000;"), hr(style = "margin: 8px 0; border-top: 1px solid #666;"), span(style = "font-size: 0.9em; font-weight: bold; color: #333;", paste("Impl:", impl_text))),
          div(style = "display: flex; align-items: center; color: #999; font-size: 1.5em;", icon("arrow-right")),
          div(style = paste0("flex: 1; border-radius: 8px; padding: 15px; border: 2px solid #ccc; background-color:", res_col, ";"), h6("Mitigated Threat / Goal", style = "font-size: 0.85em; text-transform: uppercase; color: #333; margin-bottom: 5px;"), strong(threat_label, style = "display: block; font-size: 1.0em; color: #000; word-wrap: break-word;"), hr(style = "margin: 8px 0; border-top: 1px solid #666;"), span(style = "font-size: 0.9em; font-weight: bold; color: #333;", paste("Result:", res_text))),
          div(style = "display: flex; align-items: center; color: #999; font-size: 1.5em;", icon("arrow-right")),
          div(style = "flex: 1; border-radius: 8px; padding: 15px; border: 2px solid #ccc; background-color: #E2E8F0;", h6("Conservation Targets", style = "font-size: 0.85em; text-transform: uppercase; color: #333; margin-bottom: 5px;"), strong(row$`Included Targets`, style = "display: block; font-size: 1.0em; color: #000; word-wrap: break-word;"))
      )
    })
    
    act_spatial_data <- reactive({
      req(input$action_targets_table_rows_selected)
      impl_id <- act_targets_data()[input$action_targets_table_rows_selected, "implementedactionid"]
      q <- "SELECT scale_category, ST_AsGeoJSON(geom) as geojson, ST_Area(geom::geography) * 0.000247105 AS acres FROM track.action_spatial WHERE implementedactionid = $1"
      dbGetQuery(db, q, params = list(as.integer(impl_id)))
    })
    
    output$act_map_scale <- renderText({
      df <- act_spatial_data()
      if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
    })
    
    output$act_map_area <- renderText({
      df <- act_spatial_data()
      scale <- if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
      if (scale == "Statewide") return("~66,600,000 Acres (Statewide)")
      if(nrow(df) > 0 && !is.na(df$acres)) format_acres(df$acres) else "N/A"
    })
    
    output$act_map <- renderLeaflet({
      req(input$act_nav_tabs == "spatial")
      df <- act_spatial_data()
      scale <- if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
      
      m <- leaflet() %>% setView(lng = -105.5, lat = 39.0, zoom = 6) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addLayersControl(baseGroups = c("Terrain", "Satellite"), options = layersControlOptions(collapsed = FALSE))
      
      if (scale == "Statewide") {
        m <- draw_statewide_map(m)
      } else if(nrow(df) > 0 && !is.na(df$geojson)) {
        tryCatch({
          geo_sf <- sf::st_as_sfc(df$geojson, GeoJSON = TRUE) %>% sf::st_sf(crs = 4326) %>% sf::st_make_valid()
          if(nrow(geo_sf) > 0) {
            m <- m %>% addPolygons(data = geo_sf, color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 2)
          }
        }, error = function(e) { print(e) })
      }
      m
    })
    
    
    # -----------------------------------------------------
    # TAB 3 LOGIC (ALL ACTIONS Table + Megamodal)
    # -----------------------------------------------------
    get_status_badge <- function(status) {
      if (is.null(status) || is.na(status)) return(span(class = "badge bg-secondary", "Unknown"))
      color <- switch(status,
                      "Completed" = "bg-success", "Achieved" = "bg-success", "On-track" = "bg-success",
                      "Minor issues" = "bg-warning text-dark", "Partially achieved" = "bg-warning text-dark",
                      "Major issues" = "bg-danger", "Not achieved" = "bg-danger", "Abandoned" = "bg-danger",
                      "bg-secondary")
      as.character(span(class = paste("badge", color), status))
    }
    
    all_actions_data <- reactive({
      db_sync_trigger()
      q <- "SELECT ia.implementedactionid, TO_CHAR(ia.createdon, 'MM/DD/YYYY') AS \"Date Submitted\", l2.actionl2name AS \"Action\", CASE WHEN tgts.t_count = 1 THEN tgts.first_targ WHEN tgts.t_count > 1 THEN 'Multiple' ELSE 'None' END AS \"Targets\", ia.implementation_progress AS \"Impl. Progress\" FROM track.implementedactions ia JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id LEFT JOIN (SELECT sha.implementedactionid, COUNT(sha.specieshabitatactionsid) AS t_count, MAX(CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END) AS first_targ FROM track.specieshabitatactions sha LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid GROUP BY sha.implementedactionid) tgts ON ia.implementedactionid = tgts.implementedactionid ORDER BY ia.implementedactionid DESC"
      df <- dbGetQuery(db, q)
      if(nrow(df) > 0) df$`Impl. Progress` <- sapply(df$`Impl. Progress`, get_status_badge)
      df
    })
    
    output$all_actions_table <- renderDT({
      datatable(all_actions_data(), escape = FALSE, selection = "single", rownames = F, options = list(
        dom = 'ftip', pageLength = 15, scrollY = "calc(100vh - 280px)", scrollCollapse = TRUE, info = FALSE, columnDefs = list(list(visible = F, targets = c(0)))
      ))
    })
    
    proxy_all_actions_table <- dataTableProxy("all_actions_table")
    
    observeEvent(input$all_actions_table_rows_selected, {
      req(input$all_actions_table_rows_selected)
      impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      action_name <- all_actions_data()[input$all_actions_table_rows_selected, "Action"]
      
      showModal(modalDialog(
        title = h4(strong(action_name), class="mb-0 w-100 text-start", style="color: #07234C; text-align: left;"),
        size = "xl", easyClose = TRUE, fade = TRUE,
        footer = tagList(actionButton(ns("btn_close_all_actions_modal"), "Close Window", class = "btn-secondary")),
        
        navset_underline(
          id = ns("all_actions_modal_tabs"),
          nav_panel("Action Overview", value = "overview", div(class="mt-4", layout_columns(div(class = "card shadow-sm border-0 mb-3", div(class = "card-header bg-light fw-bold", "Details"), div(class = "card-body", uiOutput(ns("all_modal_details")))), div(class = "card shadow-sm border-0 mb-3", div(class = "card-header bg-light fw-bold", "Targets & Mitigated Threats"), div(class = "card-body", uiOutput(ns("all_modal_threats"))))))),
          nav_panel("Progress Logs", value = "logs", div(class="mt-4", div(class = "card shadow-sm border-0 mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Historical Progress Logs"), div(class = "card-body", DTOutput(ns("all_modal_history_table")))))),
          nav_panel("Spatial Footprint", value = "spatial", div(class="mt-4 card shadow-sm border-0", div(class="card-body", layout_columns(div(class="p-2 text-center rounded border", style="background-color: #F8F9FA;", h6("Scale Category", class="text-muted mb-0"), h5(textOutput(ns("all_modal_map_scale")), class="mb-0 fw-bold", style="color:#0D67B8;")), div(class="p-2 text-center rounded border", style="background-color: #F8F9FA;", h6("Approx. Area", class="text-muted mb-0"), h5(textOutput(ns("all_modal_map_area")), class="mb-0 fw-bold", style="color:#055A53;"))), div(class="mt-3 border rounded", leafletOutput(ns("all_modal_map"), height="500px")))))
        )
      ))
    })
    
    observeEvent(input$btn_close_all_actions_modal, { removeModal(); selectRows(proxy_all_actions_table, NULL) })
    
    output$all_modal_details <- renderUI({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      row <- dbGetQuery(db, "SELECT actiondesc, implementation_progress, result_progress FROM track.implementedactions WHERE implementedactionid = $1", params = list(as.integer(impl_id)))
      desc <- if(is.na(row$actiondesc) || row$actiondesc=="") "No description provided." else row$actiondesc
      tagList(p(strong("Description: "), desc), p(strong("Implementation: "), HTML(get_status_badge(row$implementation_progress))), p(strong("Effectiveness: "), HTML(get_status_badge(row$result_progress))))
    })
    
    output$all_modal_threats <- renderUI({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT l2.threatl2code || '. ' || l2.threatl2name AS threat_name, ta.justification, ta.alternative_category, ta.justification_text, CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, ta.threatl2id FROM track.threatsaddressed ta JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id LEFT JOIN proj.species s ON sha.speciesid = s.speciesid LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid WHERE sha.implementedactionid = $1 ORDER BY threat_name ASC, target_name ASC"
      threats_df <- dbGetQuery(db, q, params = list(as.integer(impl_id)))
      if(nrow(threats_df) > 0) {
        threats_df$group_name <- ifelse(threats_df$threatl2id == 59, paste0("Broader Goal: ", threats_df$alternative_category), threats_df$threat_name)
        tags$ul(class = "mt-2 mb-0", lapply(unique(threats_df$group_name), function(g) {
          sub_df <- threats_df[threats_df$group_name == g, ]; is_broader <- sub_df$threatl2id[1] == 59
          title <- if(is_broader) strong("Broader Goal: ", style="color: #0D67B8;", sub_df$alternative_category[1]) else strong(sub_df$threat_name[1])
          t_list <- tags$ul(class="mt-1 mb-3", style="list-style-type:circle;", lapply(1:nrow(sub_df), function(i) tags$li(span(class="text-primary fw-bold", sub_df$target_name[i]), " - ", em(if(is_broader) sub_df$justification_text[i] else sub_df$justification[i]))))
          tags$li(class="mb-2", title, t_list)
        }))
      } else { p(em("No threats recorded.")) }
    })
    
    output$all_modal_history_table <- renderDT({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      q <- "SELECT TO_CHAR(a.actiondate, 'MM/DD/YYYY') AS \"Date\", a.implementation_progress AS \"Impl. Progress\", a.result_progress AS \"Effectiveness\", a.what_done AS \"What was done?\", COALESCE(p.first_name || ' ' || p.last_name, a.createdby) AS \"Entered By\" FROM track.actiontracking a LEFT JOIN public.profiles p ON a.createdby = p.id::text WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC"
      datatable(dbGetQuery(db, q, params=list(as.integer(impl_id))), rownames = FALSE, options = list(dom = 't', paging = FALSE, scrollY = "400px", scrollCollapse = TRUE))
    })
    
    all_modal_spatial_data <- reactive({
      req(input$all_actions_table_rows_selected); impl_id <- all_actions_data()[input$all_actions_table_rows_selected, "implementedactionid"]
      dbGetQuery(db, "SELECT scale_category, ST_AsGeoJSON(geom) as geojson, ST_Area(geom::geography) * 0.000247105 AS acres FROM track.action_spatial WHERE implementedactionid = $1", params = list(as.integer(impl_id)))
    })
    
    output$all_modal_map_scale <- renderText({ df <- all_modal_spatial_data(); if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide" })
    output$all_modal_map_area <- renderText({ 
      df <- all_modal_spatial_data()
      scale <- if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
      if (scale == "Statewide") return("~66,600,000 Acres (Statewide)")
      if(nrow(df) > 0 && !is.na(df$acres)) format_acres(df$acres) else "N/A" 
    })
    
    output$all_modal_map <- renderLeaflet({
      req(input$all_actions_modal_tabs == "spatial"); df <- all_modal_spatial_data()
      scale <- if(nrow(df) > 0 && !is.na(df$scale_category)) df$scale_category else "Statewide"
      
      m <- leaflet() %>% setView(lng = -105.5, lat = 39.0, zoom = 6) %>% addProviderTiles(providers$Esri.WorldTopoMap, group="Terrain") %>% addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>% addLayersControl(baseGroups=c("Terrain", "Satellite"), options=layersControlOptions(collapsed=FALSE))
      
      if (scale == "Statewide") {
        m <- draw_statewide_map(m)
      } else if(nrow(df) > 0 && !is.na(df$geojson)) {
        tryCatch({
          geo_sf <- sf::st_as_sfc(df$geojson, GeoJSON=TRUE) %>% sf::st_sf(crs=4326) %>% sf::st_make_valid()
          if(nrow(geo_sf) > 0) { m <- m %>% addPolygons(data=geo_sf, color="#EAB11E", fillColor="#EAB11E", fillOpacity=0.5, weight=2) }
        }, error=function(e){ print(e) })
      }
      m
    })
    
    # -----------------------------------------------------
    # TAB 4 LOGIC (Figures)
    # -----------------------------------------------------
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
    
    # --- 4A. Plotly Sankey ---
    output$sankey_plot <- renderPlotly({
      req(input$conn_nav_tabs == "sankey") # Wait until tab is active!
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
    
    # --- 4B. Interactive Chord Graph ---
    output$bipartite_plot <- renderGirafe({
      req(input$conn_nav_tabs == "bipartite") # Wait until tab is active!
      df <- links_data()
      
      q_act <- "SELECT l0a.actionl0id, TRIM(COALESCE(l0a.actionl0name, 'Unknown')) AS l0, l1a.actionl1id, TRIM(COALESCE(l1a.actionl1name, 'Unknown')) AS l1, l2a.actionl2id, TRIM(l2a.actionl2code) AS code, TRIM(l2a.actionl2name) AS name FROM proj.l2_actions l2a LEFT JOIN proj.l1_actions l1a ON l2a.actionl1id = l1a.actionl1id LEFT JOIN proj.l0_actions l0a ON l1a.actionl0id = l0a.actionl0id"
      q_thr <- "SELECT l0t.threatl0id, TRIM(COALESCE(l0t.threatl0name, 'Unknown')) AS l0, l1t.threatl1id, TRIM(COALESCE(l1t.threatl1name, 'Unknown')) AS l1, l2t.threatl2id, TRIM(l2t.threatl2code) AS code, TRIM(l2t.threatl2name) AS name FROM proj.l2_threats l2t LEFT JOIN proj.l1_threats l1t ON l2t.threatl1id = l1t.threatl1id LEFT JOIN proj.l0_threats l0t ON l1t.threatl0id = l0t.threatl0id"
      
      actions <- dbGetQuery(db, q_act) %>% filter(!l0 %in% c("D. None", "Unknown", "None"), code != "11.1") %>% 
        mutate(major_code = suppressWarnings(as.numeric(sub("\\..*", "", code))), minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", code)))) %>%
        arrange(l0, major_code, minor_code) %>% mutate(type = "Action")
      
      threats <- dbGetQuery(db, q_thr) %>% filter(!grepl("\\.9$", code), !l0 %in% c("Unknown Threats", "Unknown", "None"), threatl1id != 4) %>% 
        mutate(major_code = suppressWarnings(as.numeric(sub("\\..*", "", code))), minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", code)))) %>%
        arrange(l0, major_code, minor_code) %>% mutate(type = "Threat")
      
      n_act <- nrow(actions)
      act_angles <- seq(105 * pi/180, 255 * pi/180, length.out = max(2, n_act))
      actions <- actions %>% mutate(angle = act_angles, x = cos(angle), y = sin(angle), raw_deg = angle * 180 / pi, rad_angle = raw_deg - 180, hjust = 1)
      
      n_thr <- nrow(threats)
      thr_angles <- seq(75 * pi/180, -75 * pi/180, length.out = max(2, n_thr))
      threats <- threats %>% mutate(angle = thr_angles, x = cos(angle), y = sin(angle), raw_deg = angle * 180 / pi, rad_angle = raw_deg, hjust = 0)
      
      nodes_df <- bind_rows(actions, threats) %>% mutate(safe_id = paste0("node_", row_number()), full_text = paste(code, name, sep=" "))
      
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
      
      l1_labels <- bind_rows(
        actions %>% group_by(l1) %>% summarize(angle = mean(angle), .groups="drop") %>% mutate(r = 1.35, text_angle = angle * 180/pi - 180, hjust=1),
        threats %>% group_by(l1) %>% summarize(angle = mean(angle), .groups="drop") %>% mutate(r = 1.35, text_angle = angle * 180/pi, hjust=0)
      )
      
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
      
      bg_labels <- data.frame(x = c(-3.1, 3.1), y = c(0, 0), label = c("ACTIONS", "THREATS"), angle = c(90, 270))
      
      edges_df <- data.frame()
      if(nrow(df) > 0) {
        edges_df <- df %>%
          left_join(nodes_df %>% filter(type == "Action") %>% select(actionl2code = code, x1 = x, y1 = y), by = "actionl2code") %>%
          left_join(nodes_df %>% filter(type == "Threat") %>% select(threatl2code = code, x2 = x, y2 = y), by = "threatl2code") %>%
          mutate(hover_text = paste0("<b>Target:</b> ", target_name, " (", group_context, ")<br><b>Action:</b> ", actionl2code, " ", source_name, "<br><b>Threat:</b> ", threatl2code, " ", threat_name), safe_edge_id = paste0("edge_", row_number()))
      }
      
      p <- ggplot() +
        geom_text(data = bg_labels, aes(x=x, y=y, label=label, angle=angle), size=14, color="grey90", fontface="bold") +
        geom_segment(data=separators, aes(x=x_in, y=y_in, xend=x_out, yend=y_out), color="grey70", linewidth=0.5, linetype="dashed")
      
      if(nrow(edges_df) > 0) {
        p <- p + geom_curve_interactive(
          data = edges_df, 
          aes(x = x1, y = y1, xend = x2, yend = y2, color = group_context, linetype = target_type, tooltip = hover_text, data_id = safe_edge_id), 
          curvature = 0.1, alpha = 0.6, linewidth = 0.3
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
        theme(legend.position = "right", legend.box = "vertical", legend.title = element_text(face = "bold"), plot.caption = element_text(color = "grey40", size = 11, face = "italic", hjust = 0.5), plot.margin = margin(10, 10, 10, 10))
      
      girafe(ggobj = p, width_svg = 18, height_svg = 12,
             options = list(opts_sizing(rescale = TRUE, width = 1), opts_zoom(min = 1, max = 10), opts_toolbar(position = "topright", saveaspng = TRUE), opts_hover(css = "stroke:black;stroke-width:1.5px;cursor:pointer;stroke-opacity:1;opacity:1;"), opts_hover_inv(css = "opacity:0.8;"), opts_tooltip(css = "background-color:white;color:black;padding:10px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);")))
    })
    
    # --- 4C. Absolute Domain Matrix ---
    output$heatmap_plot <- renderPlotly({
      req(input$conn_nav_tabs == "heatmap") # Wait until tab is active!
      df <- links_data()
      
      q_act <- "SELECT l0a.actionl0id, TRIM(COALESCE(l0a.actionl0name, 'Unknown')) AS l0, l1a.actionl1id, TRIM(COALESCE(l1a.actionl1name, 'Unknown')) AS l1, l2a.actionl2id, TRIM(COALESCE(l2a.actionl2code, 'Uncoded')) AS l2c, TRIM(COALESCE(l2a.actionl2name, 'Unknown')) AS l2n FROM proj.l2_actions l2a LEFT JOIN proj.l1_actions l1a ON l2a.actionl1id = l1a.actionl1id LEFT JOIN proj.l0_actions l0a ON l1a.actionl0id = l0a.actionl0id"
      q_thr <- "SELECT l0t.threatl0id, TRIM(COALESCE(l0t.threatl0name, 'Unknown')) AS l0, l1t.threatl1id, TRIM(COALESCE(l1t.threatl1name, 'Unknown')) AS l1, l2t.threatl2id, TRIM(COALESCE(l2t.threatl2code, 'Uncoded')) AS l2c, TRIM(COALESCE(l2t.threatl2name, 'Unknown')) AS l2n FROM proj.l2_threats l2t LEFT JOIN proj.l1_threats l1t ON l2t.threatl1id = l1t.threatl1id LEFT JOIN proj.l0_threats l0t ON l1t.threatl0id = l0t.threatl0id"
      
      y_axis_df <- dbGetQuery(db, q_act) %>% filter(!l0 %in% c("D. None", "Unknown", "None"), l2c != "11.1") %>% mutate(major_code = suppressWarnings(as.numeric(sub("\\..*", "", l2c))), minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", l2c)))) %>% arrange(l0, major_code, minor_code)
      x_axis_df <- dbGetQuery(db, q_thr) %>% filter(!grepl("\\.9$", l2c), !l0 %in% c("Unknown Threats", "Unknown", "None"), threatl1id != 4) %>% mutate(major_code = suppressWarnings(as.numeric(sub("\\..*", "", l2c))), minor_code = suppressWarnings(as.numeric(sub(".*\\.", "", l2c)))) %>% arrange(l0, major_code, minor_code)
      
      y_axis_df$l2c_factor <- factor(y_axis_df$l2c, levels = unique(y_axis_df$l2c))
      x_axis_df$l2c_factor <- factor(x_axis_df$l2c, levels = unique(x_axis_df$l2c))
      
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
      
      y_axis_df$l0_z <- as.numeric(factor(y_axis_df$l0, levels = unique(y_axis_df$l0)))
      y_axis_df$l1_z <- as.numeric(factor(y_axis_df$l1, levels = unique(y_axis_df$l1))) + max(y_axis_df$l0_z)
      x_axis_df$l0_z <- as.numeric(factor(x_axis_df$l0, levels = unique(x_axis_df$l0))) + max(y_axis_df$l1_z)
      x_axis_df$l1_z <- as.numeric(factor(x_axis_df$l1, levels = unique(x_axis_df$l1))) + max(x_axis_df$l0_z)
      
      total_cats <- max(x_axis_df$l1_z)
      z_left <- cbind(y_axis_df$l0_z, y_axis_df$l1_z)
      z_bottom <- rbind(x_axis_df$l0_z, x_axis_df$l1_z)
      hover_left <- cbind(paste("Level 0 Action:", y_axis_df$l0), paste("Level 1 Action:", y_axis_df$l1))
      hover_bottom <- rbind(paste("Level 0 Threat:", x_axis_df$l0), paste("Level 1 Threat:", x_axis_df$l1))
      
      cpw_main_scale <- list(c(0, "white"), c(0.001, "#9ecae1"), c(1, "#07234c"))
      base_colors <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
      full_palette <- colorRampPalette(base_colors)(total_cats)
      axis_colorscale <- list(); for(i in 1:total_cats) { axis_colorscale[[i]] <- c((i-1)/(total_cats-1), full_palette[i]) }
      
      helper_wrap <- function(x, w) gsub("\n", "<br>", stringr::str_wrap(x, width = w))
      ann_list <- list()
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
      
      plot_ly() %>%
        add_trace(type = "heatmap", x = x_axis_df$l2c_factor, y = y_axis_df$l2c_factor, z = z_main, text = text_main, hoverinfo = "text", colorscale = cpw_main_scale, xaxis = "x", yaxis = "y", xgap = 1, ygap = 1, colorbar = list(title="<b>Distinct<br>Species/Habitats</b>", x=1.02, y=0.5, yanchor="middle", len=0.6, dtick=1)) %>%
        add_trace(type = "heatmap", x = c("L0 Action", "L1 Action"), y = y_axis_df$l2c_factor, z = z_left, text = hover_left, hoverinfo = "text", zmin=1, zmax=total_cats, colorscale = axis_colorscale, showscale = FALSE, xaxis = "x2", yaxis = "y2", xgap = 0, ygap = 0) %>%
        add_trace(type = "heatmap", x = x_axis_df$l2c_factor, y = c("L0 Threat", "L1 Threat"), z = z_bottom, text = hover_bottom, hoverinfo = "text", zmin=1, zmax=total_cats, colorscale = axis_colorscale, showscale = FALSE, xaxis = "x3", yaxis = "y3", xgap = 0, ygap = 0) %>%
        layout(plot_bgcolor = "black", paper_bgcolor = "white", annotations = ann_list, shapes = list(list(type = "rect", x0 = 0.07, x1 = 0.11, xref = "paper", y0 = 0, y1 = 1, yref = "paper", fillcolor = "white", line = list(color = "white"), layer = "below"), list(type = "rect", x0 = 0, x1 = 1, xref = "paper", y0 = 0.07, y1 = 0.11, yref = "paper", fillcolor = "white", line = list(color = "white"), layer = "below"), list(type = "rect", x0 = 0, x1 = 0.11, xref = "paper", y0 = 0, y1 = 0.11, yref = "paper", fillcolor = "white", line = list(color = "white"), layer = "below")), xaxis  = list(domain = c(0.11, 1.0), type = "category", side = "bottom", showticklabels = TRUE, tickangle = 0, tickfont = list(color="black", size=11, face="bold"), fixedrange = TRUE, title = "", range = c(-0.5, nrow(x_axis_df) - 0.5)), yaxis  = list(domain = c(0.11, 1.0), type = "category", side = "left", showticklabels = TRUE, tickfont = list(color="black", size=11, face="bold"), fixedrange = TRUE, title = "", range = c(-0.5, nrow(y_axis_df) - 0.5)), xaxis2 = list(domain = c(0.0, 0.07), type = "category", showticklabels = FALSE, fixedrange = TRUE, range = c(-0.5, 1.5)), yaxis2 = list(domain = c(0.11, 1.0), type = "category", side = "left", showticklabels = FALSE, matches = "y", fixedrange = TRUE), xaxis3 = list(domain = c(0.11, 1.0), type = "category", showticklabels = FALSE, matches = "x", fixedrange = TRUE), yaxis3 = list(domain = c(0.0, 0.07), type = "category", showticklabels = FALSE, fixedrange = TRUE, range = c(-0.5, 1.5)), margin = list(l = 150, r = 20, b = 120, t = 40)) %>% config(displayModeBar = FALSE)
    })
    
  }) 
}