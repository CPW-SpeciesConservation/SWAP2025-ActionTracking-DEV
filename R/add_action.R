library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(jsonlite)

add_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .selectize-dropdown-content .option { white-space: normal !important; word-wrap: break-word !important; padding: 8px !important; line-height: 1.3 !important; }
        .target-card { border-left: 5px solid #0D67B8; background-color: #F8F9FA; margin-bottom: 15px; padding: 15px; border-radius: 5px; }
      "))
    ),
    
    h3("Report a New Action", class = "mb-4"),
    
    # APPROACH SELECTION
    div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
        div(class = "card-header bg-primary text-white", "Approach"),
        div(class = "card-body", style = "overflow: visible;",
            radioButtons(ns("wizard_path"), "Does this action target a single species/habitat or multiple species/habitats?",
                         choices = c("Single (Species/Habitat oriented approach)" = "species", "Multiple (Actions oriented approach)" = "action"),
                         selected = character(0), inline = TRUE)
        )
    ),
    
    # ==========================================
    # PATH A: SPECIES/HABITAT ORIENTED 
    # ==========================================
    conditionalPanel(
      condition = sprintf("input['%s'] == 'species'", ns("wizard_path")),
      
      div(class = "card mb-4 shadow-sm", style = "border-color: #005A53;",
          div(class = "card-header text-white fw-bold", style = "background-color: #005A53;", "Species/Habitat Selection"),
          div(class = "card-body", style = "overflow: visible;",
              radioButtons(ns("target_type"), "Type:", choices = c("Species", "Habitat"), inline = TRUE),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Species'", ns("target_type")),
                layout_columns(
                  selectInput(ns("tax_group"), "Taxonomic Group", choices = c("Loading..." = "")),
                  selectizeInput(ns("species_select"), "Species", choices = c("Select a group first..." = ""), options = list(dropdownParent = "body"))
                )
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Habitat'", ns("target_type")),
                layout_columns(
                  selectInput(ns("major_habitat"), "Major Habitat", choices = c("Loading..." = "")),
                  selectizeInput(ns("habitat_subtype"), "Habitat Subtype", choices = c("Select a major habitat first..." = ""), options = list(dropdownParent = "body"))
                )
              )
          )
      ),
      
      conditionalPanel(
        condition = sprintf("(input['%s'] == 'Species' && input['%s'] != '') || (input['%s'] == 'Habitat' && input['%s'] != '')", ns("target_type"), ns("species_select"), ns("target_type"), ns("habitat_subtype")),
        div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
            div(class = "card-header bg-primary text-white", "Action Details & Mitigated Threats"),
            div(class = "card-body", style = "overflow: visible;",
                
                layout_columns(
                  selectInput(ns("timeframe"), "Timeframe ", choices = c("Short Term", "Long Term")),
                  selectInput(ns("implementation_progress"), 
                              label = tooltip(span("Implementation Progress ", icon("circle-info")), "What has been done? Measures the progress of implementing the action."),
                              choices = c("Not specified", "Scheduled for future", "Major issues", "Minor issues", "On-track", "Completed", "Abandoned"),
                              selected = "Scheduled for future"),
                  selectInput(ns("result_progress"), 
                              label = tooltip(span("Effectiveness Progress ", icon("circle-info")), "What has been achieved? Measures progress towards reducing the threat or achieving the goal."),
                              choices = c("Not specified", "Not Yet", "Not achieved", "Partially achieved", "On-track", "Achieved", "No longer relevant"),
                              selected = "Not Yet")
                ),
                layout_columns(
                  selectInput(ns("action_l2"), "Select Action", choices = c("Loading..." = "")),
                  selectizeInput(ns("action_detail"), "Action Detail (Optional)", choices = c("None" = "none"), options = list(dropdownParent = "body"))
                ),
                hr(),
                h5("Project Summary"),
                textAreaInput(ns("action_desc"), "General Action Description", rows = 2),
                
                # --- PATH A SPATIAL UI ---
                hr(),
                h5("Spatial Extent & Location", class = "mt-2"),
                layout_columns(
                  selectInput(ns("spatial_scale_a"), "Spatial Scale of Action", 
                              choices = c("Statewide", "Range", "Custom"), selected = "Statewide")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] != 'Statewide'", ns("spatial_scale_a")),
                  p(class = "text-muted mb-2", "If 'Range', the target's native range is shown. If 'Custom', use the polygon tool to draw your shape. Use the trash icon to delete or edit a shape."),
                  div(class = "border rounded shadow-sm mb-4", leafletOutput(ns("action_map_a"), height = "400px"))
                ),
                
                hr(),
                h5("Threats Addressed & Conservation Goals", class = "mt-4"),
                p("Select specific SWAP threats mitigated by this action, and/or define broader conservation goals. At least one must be selected."),
                
                uiOutput(ns("dynamic_threats_ui_path_a")),
                actionButton(ns("btn_submit_species"), "Submit Action", class = "btn-success btn-lg mt-4 w-100")
            )
        )
      )
    ),
    
    # ==========================================
    # PATH B: ACTION ORIENTED (NEW WORKFLOW)
    # ==========================================
    conditionalPanel(
      condition = sprintf("input['%s'] == 'action'", ns("wizard_path")),
      
      # Step 1: Action Selection
      div(class = "card mb-4 shadow-sm", style = "border-color: #005A53;",
          div(class = "card-header text-white fw-bold", style = "background-color: #005A53;", "Step 1: Action Selection"),
          div(class = "card-body",  style = "overflow: visible;",
              layout_columns(
                selectInput(ns("act_timeframe"), "Timeframe", choices = c("Short Term", "Long Term")),
                selectInput(ns("act_implementation_progress"), choices = c("Not specified", "Scheduled for future", "Major issues", "Minor issues", "On-track", "Completed", "Abandoned"), selected = "Scheduled for future", label = "Implementation Progress"),
                selectInput(ns("act_result_progress"), choices = c("Not specified", "Not Yet", "Not achieved", "Partially achieved", "On-track", "Achieved", "No longer relevant"), selected = "Not Yet", label = "Effectiveness Progress")
              ),
              layout_columns(
                selectInput(ns("act_l0"), "Level 0 Category", choices = c("Loading..." = "")),
                selectInput(ns("act_l1"), "Level 1 Category", choices = c("Select Level 0 first..." = ""))
              ),
              selectInput(ns("act_l2"), "Level 2 Action", choices = c("Select Level 1 first..." = "")),
              textAreaInput(ns("act_desc"), "General Action Description", rows = 2)
          )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] != ''", ns("act_l2")),
        
        # Step 2: Spatial Extent (Moved up for spatial filtering!)
        div(class = "card mb-4 shadow-sm", style = "border-color: #005A53;",
            div(class = "card-header text-white fw-bold", style = "background-color: #005A53;", "Step 2: Spatial Extent & Location"),
            div(class = "card-body", style = "overflow: visible;",
                layout_columns(
                  selectInput(ns("spatial_scale_b"), "Scale of Action", choices = c("Statewide", "Custom"), selected = "Statewide")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'Custom'", ns("spatial_scale_b")),
                  p(class = "text-muted mb-2", "Select the polygon tool and draw the boundaries of this action. The targets below will automatically filter to species and habitats inside this area! Use the trash icon to delete or edit a shape."),
                  div(class = "border rounded shadow-sm mb-4", leafletOutput(ns("action_map_b"), height = "400px"))
                )
            )
        ),
        
        # Step 3: Filtered Target Selection
        div(class = "card mb-4 shadow-sm", style = "border-color: #005A53;",
            div(class = "card-header text-white fw-bold", style = "background-color: #005A53;", "Step 3: Linked Targets & Mitigated Threats"),
            div(class = "card-body", style = "overflow: visible;",
                p(strong("Based on your Action and Location, select the applicable Species and Habitats this project targets. Then assign the threats mitigated for each.")),
                
                layout_columns(
                  selectizeInput(ns("act_target_species"), "Available Species Options", choices = NULL, multiple = TRUE, options = list(placeholder = "Select species...", dropdownParent = "body")),
                  selectizeInput(ns("act_target_habitats"), "Available Habitats Options", choices = NULL, multiple = TRUE, options = list(placeholder = "Select habitats...", dropdownParent = "body"))
                ),
                
                hr(),
                h5("Mitigated Threats & Action Details", class = "mt-4"),
                uiOutput(ns("dynamic_target_threats_ui_path_b")),
                
                actionButton(ns("btn_submit_action"), "Submit Action", class = "btn-success btn-lg mt-4 w-100")
            )
        )
      )
    )
  )
}

add_action_server <- function(id, db, current_user, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    
    nav_home_trigger <- reactiveVal(0)
    valid_threats_df_a <- reactiveVal(data.frame())
    
    # SHARED DROPDOWNS & INITIALIZATION
    observe({
      tax_groups <- dbGetQuery(db, "SELECT groupname FROM proj.taxonomicgroups ORDER BY groupname")
      updateSelectInput(session, "tax_group", choices = c("Choose a group..." = "", tax_groups$groupname))
      habitats <- dbGetQuery(db, "SELECT majorhabitatname FROM proj.majorhabitats ORDER BY majorhabitatname")
      updateSelectInput(session, "major_habitat", choices = c("Choose a major habitat..." = "", habitats$majorhabitatname))
      l0_acts <- dbGetQuery(db, "SELECT actionl0id, actionl0name FROM proj.l0_actions ORDER BY actionl0name")
      updateSelectInput(session, "act_l0", choices = c("Choose Level 0 Category..." = "", setNames(l0_acts$actionl0id, l0_acts$actionl0name)))
    })
    
    # ==========================================
    # PATH A: SERVER LOGIC
    # ==========================================
    observeEvent(input$tax_group, {
      req(input$tax_group) 
      query <- "SELECT speciesid, commonname, sciname FROM proj.species s JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE tg.groupname = $1 AND s.swap_rankingid IN (1, 2) ORDER BY s.commonname"
      species_list <- dbGetQuery(db, query, params = list(input$tax_group))
      choices_list <- if(nrow(species_list) > 0) setNames(species_list$speciesid, paste0(species_list$commonname, " (", species_list$sciname, ")")) else NULL
      updateSelectizeInput(session, "species_select", choices = c("Choose a species..." = "", choices_list))
    })
    
    observeEvent(input$major_habitat, {
      req(input$major_habitat) 
      query <- "SELECT habitatsubtypeid, habitatsubtypename FROM proj.habitatsubtypes hs JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE mh.majorhabitatname = $1 ORDER BY hs.habitatsubtypename"
      subtype_list <- dbGetQuery(db, query, params = list(input$major_habitat))
      choices_list <- if(nrow(subtype_list) > 0) setNames(subtype_list$habitatsubtypeid, subtype_list$habitatsubtypename) else NULL
      updateSelectizeInput(session, "habitat_subtype", choices = c("Choose a subtype..." = "", choices_list))
    })
    
    observeEvent(list(input$species_select, input$habitat_subtype, input$target_type, input$timeframe), {
      is_species <- input$target_type == "Species"
      target_id <- if(is_species) input$species_select else input$habitat_subtype
      if (is.null(target_id) || target_id == "") return()
      
      q_actions <- if(is_species) {
        "SELECT l2.actionl2id, l2.actionl2code || '. ' || l2.actionl2name AS action_name FROM xref.species_actionsl2 x JOIN proj.l2_actions l2 ON x.actionl2id = l2.actionl2id WHERE x.speciesid = $1 AND TRIM(LOWER(x.timeframe)) = TRIM(LOWER($2))"
      } else {
        "SELECT l2.actionl2id, l2.actionl2code || '. ' || l2.actionl2name AS action_name FROM xref.habitat_actionsl2 x JOIN proj.l2_actions l2 ON x.actionl2id = l2.actionl2id WHERE x.habitatsubtypeid = $1 AND TRIM(LOWER(x.timeframe)) = TRIM(LOWER($2))"
      }
      valid_actions <- dbGetQuery(db, q_actions, params = list(target_id, input$timeframe))
      if (nrow(valid_actions) > 0) updateSelectInput(session, "action_l2", choices = c("Select an Action..." = "", setNames(valid_actions$actionl2id, valid_actions$action_name)))
      else updateSelectInput(session, "action_l2", choices = c("No Actions Mapped for this Timeframe" = ""))
      
      q_details <- if(is_species) {
        "SELECT speciesactionsdetailsdistinctid AS detail_id, \"Meaningful.Details\" AS detail_text FROM proj.speciesactionsdetailsdistinct WHERE speciesid = $1 AND TRIM(LOWER(timeframe)) = TRIM(LOWER($2))"
      } else {
        "SELECT habitatactionsdetailsdistinctid AS detail_id, \"Meaningful.Details\" AS detail_text FROM proj.habitatactionsdetailsdistinct WHERE habitatsubtypeid = $1 AND TRIM(LOWER(timeframe)) = TRIM(LOWER($2))"
      }
      valid_details <- dbGetQuery(db, q_details, params = list(target_id, input$timeframe))
      if (nrow(valid_details) > 0) updateSelectizeInput(session, "action_detail", choices = c("None (Optional)" = "none", setNames(valid_details$detail_id, valid_details$detail_text)))
      else updateSelectizeInput(session, "action_detail", choices = c("No Details Found for this Timeframe" = "none"))
      
      q_threats <- if(is_species) {
        "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context
         FROM xref.species_threatsl2 x JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id
         WHERE x.speciesid = $1 AND l2.threatl2id != 59 ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)"
      } else {
        "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context
         FROM xref.habitat_threatsl2 x JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id
         WHERE x.habitatsubtypeid = $1 AND l2.threatl2id != 59 ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)"
      }
      valid_threats_df_a(dbGetQuery(db, q_threats, params = list(target_id)))
    })
    
    output$dynamic_threats_ui_path_a <- renderUI({
      vt <- valid_threats_df_a()
      swap_ui <- if (nrow(vt) == 0) { p(em("No standard SWAP threats mapped to this target."))
      } else {
        tagList(lapply(unique(vt$l0_context), function(l0) {
          sub_l0 <- vt[vt$l0_context == l0, ]
          div(class = "mb-4", h5(l0, style="color: #055A53; font-weight: bold; border-bottom: 3px solid #07234C; padding-bottom: 5px; margin-top: 10px;"),
              lapply(unique(sub_l0$l1_context), function(l1) {
                sub_l1 <- sub_l0[sub_l0$l1_context == l1, ]
                div(class = "mb-3 ms-4", h6(l1, style="color: #07234C; font-weight: bold; border-bottom: 1px solid #CCCCCC; padding-bottom: 3px; font-size: 1.05em; margin-top: 10px;"),
                    lapply(1:nrow(sub_l1), function(i) {
                      t_id <- sub_l1$threatl2id[i]
                      tagList(checkboxInput(session$ns(paste0("chk_threat_a_", t_id)), sub_l1$threat_name[i]), conditionalPanel(condition = sprintf("input['%s'] == true", session$ns(paste0("chk_threat_a_", t_id))), div(class = "ps-4 mb-3", textAreaInput(session$ns(paste0("just_threat_a_", t_id)), "Justification:", rows = 1))))
                    }))
              }))
        }))
      }
      fluidRow(
        column(6, div(class = "card shadow-sm mb-3 h-100", div(class = "card-header bg-light fw-bold", "SWAP Identified Threats"), div(class = "card-body", style = "max-height: 300px; overflow-y: auto;", swap_ui))),
        column(6, div(class = "card shadow-sm mb-3 h-100", div(class = "card-header bg-light fw-bold", "Broader Conservation Goals"), div(class = "card-body", checkboxInput(session$ns("chk_alt_a"), strong("This action addresses broader goals or alternative drivers"), value = FALSE), conditionalPanel(condition = sprintf("input['%s'] == true", session$ns("chk_alt_a")), div(class = "mt-3 p-3 rounded", style = "background-color: #F8F9FA; border: 1px solid #DEE2E6;", selectInput(session$ns("sel_alt_a"), "Primary Goal / Driver *", width = "100%", choices = c("Select a category..." = "", "A new threat has been identified", "General Conservation Support", "Supplement population declines", "Increase general public awareness")), textAreaInput(session$ns("txt_alt_a"), "Justification / Details", width = "100%", rows = 3, placeholder = "Provide additional context for this goal..."))))))
      )
    })
    
    # --- PATH A: POSTGIS MAP FETCHING ---
    output$action_map_a <- renderLeaflet({
      leaflet() %>%
        setView(lng = -105.5, lat = 39.0, zoom = 6) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "Roads") %>%
        addLayersControl(baseGroups = c("Terrain", "Satellite", "Roads"), options = layersControlOptions(collapsed = FALSE))
    })
    outputOptions(output, "action_map_a", suspendWhenHidden = FALSE)
    
    observeEvent(list(input$spatial_scale_a, input$species_select, input$habitat_subtype, input$target_type), {
      req(input$spatial_scale_a)
      
      # THE FIX: Added clearGroup and changed clearFeatures to FALSE so the group survives!
      proxy <- leafletProxy("action_map_a") %>% 
        clearShapes() %>% 
        clearGroup("drawn_features") %>% 
        removeDrawToolbar(clearFeatures = FALSE)
      
      if (input$spatial_scale_a == "Range") {
        is_species <- input$target_type == "Species"
        target_id <- if(is_species) input$species_select else input$habitat_subtype
        if (is.null(target_id) || target_id == "") return()
        
        id_notif <- showNotification("Fetching spatial ranges from database... Please wait.", type = "message", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id_notif), add = TRUE)
        
        q <- if(is_species) {
          "SELECT ST_AsGeoJSON(ST_Union(ST_MakeValid(geom))) AS geojson FROM proj.species_ranges WHERE speciesid = $1"
        } else {
          "SELECT ST_AsGeoJSON(ST_Union(ST_MakeValid(geom))) AS geojson FROM proj.habitat_ranges WHERE habitatsubtypeid = $1"
        }
        res <- dbGetQuery(db, q, params = list(as.integer(target_id)))
        
        if (nrow(res) > 0 && !is.na(res$geojson[1])) {
          geo_sfc <- sf::st_as_sfc(res$geojson[1], GeoJSON = TRUE)
          geo_sf <- sf::st_sf(geom = geo_sfc, crs = 4326)
          
          proxy %>% addPolygons(data = geo_sf, color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 2)
        } else {
          showNotification("No range map available for this selection.", type = "warning")
        }
        
      } else if (input$spatial_scale_a == "Custom") {
        proxy %>% addDrawToolbar(
          targetGroup = "drawn_features", polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE,
          markerOptions = drawMarkerOptions(), polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions = drawShapeOptions(color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 3)),
          editOptions = editToolbarOptions(edit = TRUE, remove = TRUE) 
        )
      }
    })
    
    # Path A Submit
    observeEvent(input$btn_submit_species, {
      vt <- valid_threats_df_a()
      selected_threats <- c()
      for(t_id in vt$threatl2id) { if(isTRUE(input[[paste0("chk_threat_a_", t_id)]])) selected_threats <- c(selected_threats, t_id) }
      alt_checked <- isTRUE(input$chk_alt_a)
      
      if (input$action_l2 == "") { showNotification("Please select an Action.", type = "error"); return() }
      if (length(selected_threats) == 0 && !alt_checked) { showNotification("Please select at least one SWAP Threat OR a Broader Conservation Goal.", type = "error"); return() }
      for (t_id in selected_threats) { if (is.null(input[[paste0("just_threat_a_", t_id)]]) || trimws(input[[paste0("just_threat_a_", t_id)]]) == "") { showNotification("Please provide a justification for all selected standard threats.", type = "error"); return() } }
      if (alt_checked && (is.null(input$sel_alt_a) || input$sel_alt_a == "")) { showNotification("Please select a Primary Goal.", type = "error"); return() }
      
      # Validate: Custom scale requires a drawn polygon
      if (input$spatial_scale_a == "Custom" && (is.null(input$action_map_a_draw_all_features) || length(input$action_map_a_draw_all_features$features) == 0)) {
        showNotification("You chose 'Custom' spatial scale but did not draw a polygon. Please draw a boundary on the map, or change the scale to 'Statewide' or 'Range'.", type = "error", duration = 8)
        return()
      }
      
      geom_json <- NA_character_
      if (input$spatial_scale_a == "Custom" && !is.null(input$action_map_a_draw_all_features) && length(input$action_map_a_draw_all_features$features) > 0) {
        geom_json <- as.character(jsonlite::toJSON(input$action_map_a_draw_all_features$features[[1]]$geometry, auto_unbox = TRUE))
      } else if (input$spatial_scale_a == "Range") {
        is_species <- input$target_type == "Species"
        target_id <- as.integer(if(is_species) input$species_select else input$habitat_subtype)
        
        q_range <- if(is_species) {
          "SELECT ST_AsGeoJSON(ST_Union(ST_MakeValid(geom))) AS geojson FROM proj.species_ranges WHERE speciesid = $1"
        } else {
          "SELECT ST_AsGeoJSON(ST_Union(ST_MakeValid(geom))) AS geojson FROM proj.habitat_ranges WHERE habitatsubtypeid = $1"
        }
        res_range <- dbGetQuery(db, q_range, params = list(target_id))
        if (nrow(res_range) > 0 && !is.na(res_range$geojson[1])) {
          geom_json <- res_range$geojson[1]
        }
      }
      
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          is_species <- input$target_type == "Species"
          target_id <- as.integer(if(is_species) input$species_select else input$habitat_subtype)
          detail_val <- if(is.null(input$action_detail) || input$action_detail == "none" || input$action_detail == "") NA_integer_ else as.integer(input$action_detail)
          
          q1 <- "INSERT INTO track.implementedactions (actionl2id, timeframe, actiondesc, implementation_progress, result_progress, createdby) VALUES ($1, $2, $3, $4, $5, $6) RETURNING implementedactionid"
          new_impl_id <- dbGetQuery(conn, q1, params = list(as.integer(input$action_l2), input$timeframe, input$action_desc, input$implementation_progress, input$result_progress, current_user()$user_id))$implementedactionid[1]
          
          q2 <- if (is_species) "INSERT INTO track.specieshabitatactions (specieshabitat, speciesid, speciesactiondetailid, implementedactionid, createdby) VALUES (TRUE, $1, $2, $3, $4) RETURNING specieshabitatactionsid" else "INSERT INTO track.specieshabitatactions (specieshabitat, habitatsubtypeid, habitatactiondetailid, implementedactionid, createdby) VALUES (FALSE, $1, $2, $3, $4) RETURNING specieshabitatactionsid"
          new_sha_id <- dbGetQuery(conn, q2, params = list(target_id, detail_val, new_impl_id, current_user()$user_id))$specieshabitatactionsid[1]
          
          if (!is.na(geom_json)) dbExecute(conn, "INSERT INTO track.action_spatial (implementedactionid, scale_category, geom) VALUES ($1, $2, ST_SetSRID(ST_GeomFromGeoJSON($3), 4326))", params = list(new_impl_id, input$spatial_scale_a, geom_json))
          else dbExecute(conn, "INSERT INTO track.action_spatial (implementedactionid, scale_category) VALUES ($1, $2)", params = list(new_impl_id, input$spatial_scale_a))
          
          q3 <- "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, createdby, justification) VALUES ($1, $2, $3, $4)"
          for (t_id in selected_threats) { dbExecute(conn, q3, params = list(new_sha_id, as.integer(t_id), current_user()$user_id, input[[paste0("just_threat_a_", t_id)]])) }
          if (alt_checked) dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, alternative_category, justification_text, createdby) VALUES ($1, $2, $3, $4, $5)", params = list(new_sha_id, 59, input$sel_alt_a, input$txt_alt_a, current_user()$user_id))
        })
        db_sync_trigger(db_sync_trigger() + 1)
        showNotification("Success! Action recorded.", type = "message", duration = 5)
        
        # --- FULL SLATE WIPE (PATH A) ---
        updateRadioButtons(session, "wizard_path", selected = character(0))
        updateRadioButtons(session, "target_type", selected = "Species")
        updateSelectInput(session, "tax_group", selected = "")
        updateSelectizeInput(session, "species_select", selected = "")
        updateSelectInput(session, "major_habitat", selected = "")
        updateSelectizeInput(session, "habitat_subtype", selected = "")
        updateSelectInput(session, "timeframe", selected = "Short Term")
        updateSelectInput(session, "implementation_progress", selected = "Scheduled for future")
        updateSelectInput(session, "result_progress", selected = "Not Yet")
        updateSelectInput(session, "action_l2", choices = c("Loading..." = ""), selected = "")
        updateSelectizeInput(session, "action_detail", choices = c("None" = "none"), selected = "none")
        updateTextAreaInput(session, "action_desc", value = "")
        updateSelectInput(session, "spatial_scale_a", selected = "Statewide")
        updateCheckboxInput(session, "chk_alt_a", value = FALSE)
        updateSelectInput(session, "sel_alt_a", selected = "")
        updateTextAreaInput(session, "txt_alt_a", value = "")
        
        # THE FIX: Added clearGroup and changed clearFeatures to FALSE for the wipe!
        leafletProxy("action_map_a") %>% clearShapes() %>% clearGroup("drawn_features") %>% removeDrawToolbar(clearFeatures = FALSE)
        
        nav_home_trigger(nav_home_trigger() + 1)
      }, error = function(e) { showNotification(paste("Database Error:", e$message), type = "error", duration = 10) })
    })
    
    # ==========================================
    # PATH B: POSTGIS SPATIAL FILTERING
    # ==========================================
    observeEvent(input$act_l0, {
      req(input$act_l0)
      q <- "SELECT actionl1id, actionl1code || '. ' || actionl1name AS n FROM proj.l1_actions WHERE actionl0id = $1"
      res <- dbGetQuery(db, q, params = list(input$act_l0))
      updateSelectInput(session, "act_l1", choices = c("Select Level 1 Category..." = "", setNames(res$actionl1id, res$n)))
    })
    
    observeEvent(input$act_l1, {
      req(input$act_l1)
      q <- "SELECT actionl2id, actionl2code || '. ' || actionl2name AS n FROM proj.l2_actions WHERE actionl1id = $1"
      res <- dbGetQuery(db, q, params = list(input$act_l1))
      updateSelectInput(session, "act_l2", choices = c("Select Level 2 Action..." = "", setNames(res$actionl2id, res$n)))
    })
    
    output$action_map_b <- renderLeaflet({
      leaflet() %>% 
        setView(lng = -105.5, lat = 39.0, zoom = 6) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "Roads") %>%
        addLayersControl(baseGroups = c("Terrain", "Satellite", "Roads"), options = layersControlOptions(collapsed = FALSE))
    })
    outputOptions(output, "action_map_b", suspendWhenHidden = FALSE)
    
    observeEvent(input$spatial_scale_b, {
      req(input$spatial_scale_b)
      
      # THE FIX: Added clearGroup and changed clearFeatures to FALSE so the group survives!
      proxy <- leafletProxy("action_map_b") %>% 
        clearShapes() %>% 
        clearGroup("drawn_features") %>% 
        removeDrawToolbar(clearFeatures = FALSE)
      
      if (input$spatial_scale_b == "Custom") {
        proxy %>% addDrawToolbar(
          targetGroup = "drawn_features", polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE,
          markerOptions = drawMarkerOptions(), polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions = drawShapeOptions(color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 3)),
          editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
        )
      }
    })
    
    # THE MAGIC FILTER: Updates when L2 changes OR a new shape is drawn
    observeEvent(list(input$act_l2, input$act_timeframe, input$spatial_scale_b, input$action_map_b_draw_all_features), {
      req(input$act_l2, input$act_timeframe)
      
      q_sp <- "SELECT s.speciesid, s.commonname, tg.groupname FROM xref.species_actionsl2 x JOIN proj.species s ON x.speciesid = s.speciesid JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE x.actionl2id = $1 AND TRIM(LOWER(x.timeframe)) = TRIM(LOWER($2)) AND s.swap_rankingid IN (1, 2)"
      base_sp <- dbGetQuery(db, q_sp, params = list(input$act_l2, input$act_timeframe))
      
      q_hab <- "SELECT hs.habitatsubtypeid, hs.habitatsubtypename, mh.majorhabitatname FROM xref.habitat_actionsl2 x JOIN proj.habitatsubtypes hs ON x.habitatsubtypeid = hs.habitatsubtypeid JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE x.actionl2id = $1 AND TRIM(LOWER(x.timeframe)) = TRIM(LOWER($2))"
      base_hab <- dbGetQuery(db, q_hab, params = list(input$act_l2, input$act_timeframe))
      
      if (input$spatial_scale_b == "Custom" && !is.null(input$action_map_b_draw_all_features) && length(input$action_map_b_draw_all_features$features) > 0) {
        
        id_notif <- showNotification("Filtering targets via spatial intersection... Please wait.", type = "message", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id_notif), add = TRUE)
        
        geom_json <- as.character(jsonlite::toJSON(input$action_map_b_draw_all_features$features[[1]]$geometry, auto_unbox = TRUE))
        
        if (nrow(base_sp) > 0) {
          q_sp_spatial <- "SELECT DISTINCT speciesid FROM proj.species_ranges WHERE ST_Intersects(geom, ST_SetSRID(ST_GeomFromGeoJSON($1), 4326))"
          spatial_sp <- dbGetQuery(db, q_sp_spatial, params = list(geom_json))
          base_sp <- base_sp[base_sp$speciesid %in% spatial_sp$speciesid, ]
        }
        if (nrow(base_hab) > 0) {
          q_hab_spatial <- "SELECT DISTINCT habitatsubtypeid FROM proj.habitat_ranges WHERE ST_Intersects(geom, ST_SetSRID(ST_GeomFromGeoJSON($1), 4326))"
          spatial_hab <- dbGetQuery(db, q_hab_spatial, params = list(geom_json))
          base_hab <- base_hab[base_hab$habitatsubtypeid %in% spatial_hab$habitatsubtypeid, ]
        }
      }
      
      if (nrow(base_sp) > 0) { updateSelectizeInput(session, "act_target_species", choices = split(setNames(base_sp$speciesid, base_sp$commonname), base_sp$groupname), options = list(placeholder = "Select species inside action area...", dropdownParent = "body"))
      } else { updateSelectizeInput(session, "act_target_species", choices = character(0), options = list(placeholder = "No species mapped in this area/action.", dropdownParent = "body")) }
      
      if (nrow(base_hab) > 0) { updateSelectizeInput(session, "act_target_habitats", choices = split(setNames(base_hab$habitatsubtypeid, base_hab$habitatsubtypename), base_hab$majorhabitatname), options = list(placeholder = "Select habitats inside action area...", dropdownParent = "body"))
      } else { updateSelectizeInput(session, "act_target_habitats", choices = character(0), options = list(placeholder = "No habitats mapped in this area/action.", dropdownParent = "body")) }
    })
    
    output$dynamic_target_threats_ui_path_b <- renderUI({
      sel_sp <- input$act_target_species
      sel_hab <- input$act_target_habitats
      timeframe <- input$act_timeframe
      if (length(sel_sp) == 0 && length(sel_hab) == 0) return(p("Please select targets above to view available threats."))
      ui_blocks <- list()
      
      if (length(sel_sp) > 0) {
        for (sp_id in sel_sp) {
          sp_name <- dbGetQuery(db, "SELECT commonname FROM proj.species WHERE speciesid = $1", params = list(sp_id))$commonname[1]
          threats <- dbGetQuery(db, "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context FROM xref.species_threatsl2 x JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id WHERE x.speciesid = $1 AND l2.threatl2id != 59 ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)", params = list(sp_id))
          det_df <- dbGetQuery(db, "SELECT speciesactionsdetailsdistinctid AS detail_id, \"Meaningful.Details\" AS detail_text FROM proj.speciesactionsdetailsdistinct WHERE speciesid = $1 AND TRIM(LOWER(timeframe)) = TRIM(LOWER($2))", params = list(sp_id, timeframe))
          det_choices <- if(nrow(det_df) > 0) setNames(det_df$detail_id, det_df$detail_text) else NULL
          
          swap_ui <- if(nrow(threats) > 0) {
            tagList(lapply(unique(threats$l0_context), function(l0) {
              sub_l0 <- threats[threats$l0_context == l0, ]; div(class = "mb-3", h6(l0, style="color: #055A53; font-weight: bold; border-bottom: 2px solid #07234C; padding-bottom: 4px; margin-top: 15px;"), lapply(unique(sub_l0$l1_context), function(l1) {
                sub_l1 <- sub_l0[sub_l0$l1_context == l1, ]; div(class = "mb-2 ms-4", h6(l1, style="color: #07234C; font-weight: bold; border-bottom: 1px solid #CCCCCC; padding-bottom: 3px; font-size: 1.05em; margin-top: 10px;"), lapply(1:nrow(sub_l1), function(i) {
                  t_id <- sub_l1$threatl2id[i]; chk_id <- paste0("act_chk_sp_", sp_id, "_", t_id); txt_id <- paste0("act_txt_sp_", sp_id, "_", t_id)
                  tagList(checkboxInput(session$ns(chk_id), sub_l1$threat_name[i]), conditionalPanel(condition = sprintf("input['%s'] == true", session$ns(chk_id)), div(class = "ps-4 mb-2", textAreaInput(session$ns(txt_id), "Justification:", rows = 1))))
                }))
              }))
            }))
          } else { p(em("No standard SWAP threats mapped.")) }
          alt_ui <- div(class = "mt-3", checkboxInput(session$ns(paste0("act_chk_alt_sp_", sp_id)), strong("This action addresses broader goals or alternative drivers"), value = FALSE), conditionalPanel(condition = sprintf("input['%s'] == true", session$ns(paste0("act_chk_alt_sp_", sp_id))), div(class = "mt-2 p-3 rounded", style = "background-color: #F8F9FA; border: 1px solid #DEE2E6;", selectInput(session$ns(paste0("act_sel_alt_sp_", sp_id)), "Primary Goal / Driver *", width = "100%", choices = c("Select a category..." = "", "A new threat has been identified", "General Conservation Support", "Supplement population declines", "Increase general public awareness")), textAreaInput(session$ns(paste0("act_txt_alt_sp_", sp_id)), "Justification / Details", width = "100%", rows = 2))))
          ui_blocks[[length(ui_blocks) + 1]] <- div(class = "target-card mb-4 p-3 border rounded shadow-sm bg-white", 
                                                    div(class = "d-flex flex-wrap align-items-center gap-3 mb-3",
                                                        h5(sp_name, class = "text-primary fw-bold mb-0", style = "min-width: 250px;"),
                                                        div(style = "flex-grow: 1;", selectInput(session$ns(paste0("act_detail_sp_", sp_id)), "Specific Action Detail (Optional):", choices = c("None" = "", det_choices), width = "100%"))
                                                    ), 
                                                    fluidRow(
                                                      column(6, div(class = "card shadow-sm mb-3 h-100", div(class = "card-header bg-light fw-bold", "SWAP Identified Threats"), div(class = "card-body", style = "max-height: 250px; overflow-y: auto;", swap_ui))), 
                                                      column(6, div(class = "card shadow-sm mb-3 h-100", div(class = "card-header bg-light fw-bold", "Broader Conservation Goals"), div(class = "card-body", alt_ui)))
                                                    )
          )
        }
      }
      if (length(sel_hab) > 0) {
        for (hab_id in sel_hab) {
          hab_name <- dbGetQuery(db, "SELECT habitatsubtypename FROM proj.habitatsubtypes WHERE habitatsubtypeid = $1", params = list(hab_id))$habitatsubtypename[1]
          threats <- dbGetQuery(db, "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context FROM xref.habitat_threatsl2 x JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id WHERE x.habitatsubtypeid = $1 AND l2.threatl2id != 59 ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)", params = list(hab_id))
          det_df <- dbGetQuery(db, "SELECT habitatactionsdetailsdistinctid AS detail_id, \"Meaningful.Details\" AS detail_text FROM proj.habitatactionsdetailsdistinct WHERE habitatsubtypeid = $1 AND TRIM(LOWER(timeframe)) = TRIM(LOWER($2))", params = list(hab_id, timeframe))
          det_choices <- if(nrow(det_df) > 0) setNames(det_df$detail_id, det_df$detail_text) else NULL
          
          swap_ui <- if(nrow(threats) > 0) {
            tagList(lapply(unique(threats$l0_context), function(l0) {
              sub_l0 <- threats[threats$l0_context == l0, ]; div(class = "mb-3", h6(l0, style="color: #055A53; font-weight: bold; border-bottom: 2px solid #07234C; padding-bottom: 4px; margin-top: 15px;"), lapply(unique(sub_l0$l1_context), function(l1) {
                sub_l1 <- sub_l0[sub_l0$l1_context == l1, ]; div(class = "mb-2 ms-4", h6(l1, style="color: #07234C; font-weight: bold; border-bottom: 1px solid #CCCCCC; padding-bottom: 3px; font-size: 1.05em; margin-top: 10px;"), lapply(1:nrow(sub_l1), function(i) {
                  t_id <- sub_l1$threatl2id[i]; chk_id <- paste0("act_chk_hab_", hab_id, "_", t_id); txt_id <- paste0("act_txt_hab_", hab_id, "_", t_id)
                  tagList(checkboxInput(session$ns(chk_id), sub_l1$threat_name[i]), conditionalPanel(condition = sprintf("input['%s'] == true", session$ns(chk_id)), div(class = "ps-4 mb-2", textAreaInput(session$ns(txt_id), "Justification:", rows = 1))))
                }))
              }))
            }))
          } else { p(em("No standard SWAP threats mapped.")) }
          alt_ui <- div(class = "mt-3", checkboxInput(session$ns(paste0("act_chk_alt_hab_", hab_id)), strong("This action addresses broader goals or alternative drivers"), value = FALSE), conditionalPanel(condition = sprintf("input['%s'] == true", session$ns(paste0("act_chk_alt_hab_", hab_id))), div(class = "mt-2 p-3 rounded", style = "background-color: #F8F9FA; border: 1px solid #DEE2E6;", selectInput(session$ns(paste0("act_sel_alt_hab_", hab_id)), "Primary Goal / Driver *", width = "100%", choices = c("Select a category..." = "", "A new threat has been identified", "General Conservation Support", "Supplement population declines", "Increase general public awareness")), textAreaInput(session$ns(paste0("act_txt_alt_hab_", hab_id)), "Justification / Details", width = "100%", rows = 2))))
          ui_blocks[[length(ui_blocks) + 1]] <- div(class = "target-card mb-4 p-3 border rounded shadow-sm bg-white", 
                                                    div(class = "d-flex flex-wrap align-items-center gap-3 mb-3",
                                                        h5(hab_name, class = "text-success fw-bold mb-0", style = "min-width: 250px;"),
                                                        div(style = "flex-grow: 1;", selectInput(session$ns(paste0("act_detail_hab_", hab_id)), "Specific Action Detail (Optional):", choices = c("None" = "", det_choices), width = "100%"))
                                                    ), 
                                                    fluidRow(
                                                      column(6, div(class = "card shadow-sm mb-3 h-100", div(class = "card-header bg-light fw-bold", "SWAP Identified Threats"), div(class = "card-body", style = "max-height: 250px; overflow-y: auto;", swap_ui))), 
                                                      column(6, div(class = "card shadow-sm mb-3 h-100", div(class = "card-header bg-light fw-bold", "Broader Conservation Goals"), div(class = "card-body", alt_ui)))
                                                    )
          )
        }
      }
      return(tagList(ui_blocks))
    })
    
    # Path B Submit
    observeEvent(input$btn_submit_action, {
      sel_sp <- input$act_target_species
      sel_hab <- input$act_target_habitats
      if (length(sel_sp) == 0 && length(sel_hab) == 0) { showNotification("Please select at least one target.", type = "error"); return() }
      
      for (sp_id in sel_sp) {
        threats      <- dbGetQuery(db, "SELECT threatl2id FROM xref.species_threatsl2 WHERE speciesid = $1 AND threatl2id != 59", params = list(sp_id))
        any_standard <- any(sapply(threats$threatl2id, function(t_id) isTRUE(input[[paste0("act_chk_sp_", sp_id, "_", t_id)]])))
        alt_checked  <- isTRUE(input[[paste0("act_chk_alt_sp_", sp_id)]])
        if (!any_standard && !alt_checked) {
          showNotification("Please select at least one threat or broader goal for all selected species.", type = "error")
          return()
        }
      }
      
      for (hab_id in sel_hab) {
        threats      <- dbGetQuery(db, "SELECT threatl2id FROM xref.habitat_threatsl2 WHERE habitatsubtypeid = $1 AND threatl2id != 59", params = list(hab_id))
        any_standard <- any(sapply(threats$threatl2id, function(t_id) isTRUE(input[[paste0("act_chk_hab_", hab_id, "_", t_id)]])))
        alt_checked  <- isTRUE(input[[paste0("act_chk_alt_hab_", hab_id)]])
        if (!any_standard && !alt_checked) {
          showNotification("Please select at least one threat or broader goal for all selected habitats.", type = "error")
          return()
        }
      }
      # Validate: Custom scale requires a drawn polygon
      if (input$spatial_scale_b == "Custom" && (is.null(input$action_map_b_draw_all_features) || length(input$action_map_b_draw_all_features$features) == 0)) {
        showNotification("You chose 'Custom' spatial scale but did not draw a polygon. Please draw a boundary on the map, or change the scale to 'Statewide'.", type = "error", duration = 8)
        return()
      }
      
      geom_json <- NA_character_
      if (input$spatial_scale_b == "Custom" && !is.null(input$action_map_b_draw_all_features) && length(input$action_map_b_draw_all_features$features) > 0) geom_json <- as.character(jsonlite::toJSON(input$action_map_b_draw_all_features$features[[1]]$geometry, auto_unbox = TRUE))
      
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          q1 <- "INSERT INTO track.implementedactions (actionl2id, timeframe, actiondesc, implementation_progress, result_progress, createdby) VALUES ($1, $2, $3, $4, $5, $6) RETURNING implementedactionid"
          new_impl_id <- dbGetQuery(conn, q1, params = list(as.integer(input$act_l2), input$act_timeframe, input$act_desc, input$act_implementation_progress, input$act_result_progress, current_user()$user_id))$implementedactionid[1]
          
          if (!is.na(geom_json)) dbExecute(conn, "INSERT INTO track.action_spatial (implementedactionid, scale_category, geom) VALUES ($1, $2, ST_SetSRID(ST_GeomFromGeoJSON($3), 4326))", params = list(new_impl_id, input$spatial_scale_b, geom_json))
          else dbExecute(conn, "INSERT INTO track.action_spatial (implementedactionid, scale_category) VALUES ($1, $2)", params = list(new_impl_id, input$spatial_scale_b))
          
          if (length(sel_sp) > 0) { for (sp_id in sel_sp) { raw_det <- input[[paste0("act_detail_sp_", sp_id)]]; det_val <- if(is.null(raw_det) || raw_det == "") NA_integer_ else as.integer(raw_det); new_sha_id <- dbGetQuery(conn, "INSERT INTO track.specieshabitatactions (specieshabitat, speciesid, speciesactiondetailid, implementedactionid, createdby) VALUES (TRUE, $1, $2, $3, $4) RETURNING specieshabitatactionsid", params = list(sp_id, det_val, new_impl_id, current_user()$user_id))$specieshabitatactionsid[1]; threats <- dbGetQuery(conn, "SELECT threatl2id FROM xref.species_threatsl2 WHERE speciesid = $1 AND threatl2id != 59", params = list(sp_id)); for (t_id in threats$threatl2id) { if (isTRUE(input[[paste0("act_chk_sp_", sp_id, "_", t_id)]])) dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, createdby, justification) VALUES ($1, $2, $3, $4)", params = list(new_sha_id, t_id, current_user()$user_id, input[[paste0("act_txt_sp_", sp_id, "_", t_id)]])) }; if (isTRUE(input[[paste0("act_chk_alt_sp_", sp_id)]])) dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, alternative_category, justification_text, createdby) VALUES ($1, $2, $3, $4, $5)", params = list(new_sha_id, 59, input[[paste0("act_sel_alt_sp_", sp_id)]], input[[paste0("act_txt_alt_sp_", sp_id)]], current_user()$user_id)) } }
          if (length(sel_hab) > 0) { for (hab_id in sel_hab) { raw_det <- input[[paste0("act_detail_hab_", hab_id)]]; det_val <- if(is.null(raw_det) || raw_det == "") NA_integer_ else as.integer(raw_det); new_sha_id <- dbGetQuery(conn, "INSERT INTO track.specieshabitatactions (specieshabitat, habitatsubtypeid, habitatactiondetailid, implementedactionid, createdby) VALUES (FALSE, $1, $2, $3, $4) RETURNING specieshabitatactionsid", params = list(hab_id, det_val, new_impl_id, current_user()$user_id))$specieshabitatactionsid[1]; threats <- dbGetQuery(conn, "SELECT threatl2id FROM xref.habitat_threatsl2 WHERE habitatsubtypeid = $1 AND threatl2id != 59", params = list(hab_id)); for (t_id in threats$threatl2id) { if (isTRUE(input[[paste0("act_chk_hab_", hab_id, "_", t_id)]])) dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, createdby, justification) VALUES ($1, $2, $3, $4)", params = list(new_sha_id, t_id, current_user()$user_id, input[[paste0("act_txt_hab_", hab_id, "_", t_id)]])) }; if (isTRUE(input[[paste0("act_chk_alt_hab_", hab_id)]])) dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, alternative_category, justification_text, createdby) VALUES ($1, $2, $3, $4, $5)", params = list(new_sha_id, 59, input[[paste0("act_sel_alt_hab_", hab_id)]], input[[paste0("act_txt_alt_hab_", hab_id)]], current_user()$user_id)) } }
        })
        db_sync_trigger(db_sync_trigger() + 1)
        showNotification("Success! Action recorded.", type = "message", duration = 5)
        
        # --- FULL SLATE WIPE (PATH B) ---
        updateRadioButtons(session, "wizard_path", selected = character(0))
        updateSelectInput(session, "act_timeframe", selected = "Short Term")
        updateSelectInput(session, "act_implementation_progress", selected = "Scheduled for future")
        updateSelectInput(session, "act_result_progress", selected = "Not Yet")
        updateSelectInput(session, "act_l0", selected = "")
        updateSelectInput(session, "act_l1", choices = c("Select Level 0 first..." = ""), selected = "")
        updateSelectInput(session, "act_l2", choices = c("Select Level 1 first..." = ""), selected = "")
        updateTextAreaInput(session, "act_desc", value = "")
        updateSelectInput(session, "spatial_scale_b", selected = "Statewide")
        updateSelectizeInput(session, "act_target_species", selected = character(0))
        updateSelectizeInput(session, "act_target_habitats", selected = character(0))
        
        # THE FIX: Added clearGroup and changed clearFeatures to FALSE for the wipe!
        leafletProxy("action_map_b") %>% clearShapes() %>% clearGroup("drawn_features") %>% removeDrawToolbar(clearFeatures = FALSE)
        
        nav_home_trigger(nav_home_trigger() + 1)
      }, error = function(e) { showNotification(paste("Database Error:", e$message), type = "error", duration = 10) })
    })
    
    return(list(go_home = nav_home_trigger))
  })
}