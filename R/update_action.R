library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(jsonlite)
library(DT)

update_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("main_list_view"),
        h3("Update an Existing Action", class = "mb-2"),
        p("Click on any action in your directory to open the Management Dashboard, log progress, or update spatial bounds.", class = "text-muted mb-4"),
        
        div(class = "card shadow-sm border-0",
            div(class = "card-header text-white fw-bold", style = "background-color: #07234C;", "Your Action Directory"),
            div(class = "card-body",
                DTOutput(ns("action_table"))
            )
        )
    )
  )
}

update_action_server <- function(id, db, current_user, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper for color-coded status badges
    get_status_badge <- function(status) {
      if (is.null(status) || is.na(status)) return(span(class = "badge bg-secondary", "Unknown"))
      color <- switch(status,
                      "Completed" = "bg-success", "Achieved" = "bg-success", "On-track" = "bg-success",
                      "Minor issues" = "bg-warning text-dark", "Partially achieved" = "bg-warning text-dark",
                      "Major issues" = "bg-danger", "Not achieved" = "bg-danger", "Abandoned" = "bg-danger",
                      "bg-secondary")
      span(class = paste("badge", color), status)
    }
    
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
        ORDER BY ia.implementedactionid DESC
      "
      dbGetQuery(db, query, params = list(current_user()$user_id, current_user()$user_id))
    })
    
    output$action_table <- renderDT({
      datatable(isolate(action_data()), selection = "single", rownames = FALSE, 
                options = list(pageLength = 10, dom = 'ftp', columnDefs = list(list(visible = FALSE, targets = c(0, 6)))))
    })
    
    proxy_action_table <- dataTableProxy("action_table")
    observeEvent(action_data(), {
      replaceData(proxy_action_table, action_data(), resetPaging = FALSE, clearSelection = "none", rownames = FALSE)
    }, ignoreInit = TRUE)
    
    # --- STATE TRACKERS ---
    preloaded_geom <- reactiveVal(NULL)
    last_drawn_scale <- reactiveVal(NULL)
    
    # ==========================================
    # THE MEGAMODAL LOGIC
    # ==========================================
    observeEvent(input$action_table_rows_selected, {
      req(input$action_table_rows_selected)
      selected_row <- action_data()[input$action_table_rows_selected, ]
      impl_id <- selected_row$implementedactionid
      
      q_spatial <- "SELECT scale_category, ST_AsGeoJSON(geom) AS geojson FROM track.action_spatial WHERE implementedactionid = $1"
      spatial_df <- dbGetQuery(db, q_spatial, params = list(as.integer(impl_id)))
      current_scale <- if(nrow(spatial_df) > 0 && !is.na(spatial_df$scale_category)) spatial_df$scale_category else "Statewide"
      current_geom <- if(nrow(spatial_df) > 0 && !is.na(spatial_df$geojson)) spatial_df$geojson else NULL
      
      q_count <- "SELECT count(*) AS n FROM track.specieshabitatactions WHERE implementedactionid = $1"
      target_count <- dbGetQuery(db, q_count, params = list(as.integer(impl_id)))$n[1]
      allowed_scales <- if(target_count > 1) c("Statewide", "Custom") else c("Statewide", "Range", "Custom")
      
      showModal(modalDialog(
        title = h4(strong(selected_row$Action), class="mb-0", style="color: #07234C;"),
        size = "xl", easyClose = FALSE, fade = TRUE,
        footer = tagList(actionButton(ns("btn_close_modal"), "Close Dashboard", class = "btn-secondary")),
        
        navset_underline(
          id = ns("modal_tabs"),
          
          # --- TAB 1: OVERVIEW & THREATS ---
          nav_panel("Action Overview", value = "overview",
                    div(class = "mt-4",
                        layout_columns(
                          div(class = "card shadow-sm border-0 mb-3", div(class = "card-header bg-light fw-bold", "Details"), div(class = "card-body", uiOutput(ns("modal_core_details")))),
                          div(class = "card shadow-sm border-0 mb-3", div(class = "card-header bg-light fw-bold", "Delegation & Permissions"), div(class = "card-body", uiOutput(ns("collaborators_ui"))))
                        ),
                        div(class = "card shadow-sm border-0", div(class = "card-header bg-light fw-bold", "Targets & Mitigated Threats"), div(class = "card-body", uiOutput(ns("modal_threats_details"))))
                    )
          ),
          
          # --- TAB 2: PROGRESS LOG ---
          nav_panel("Log Update & Progress", value = "progress",
                    div(class = "mt-4",
                        layout_columns(
                          div(class = "card shadow-sm border-0 mb-3", div(class = "card-header text-white fw-bold", style = "background-color: #055A53;", "Historical Progress Logs"), div(class = "card-body", DTOutput(ns("history_table")))),
                          div(class = "card shadow-sm border-0 mb-3", style = "border-top: 4px solid #0D67B8 !important;",
                              div(class = "card-header bg-light fw-bold", "Submit New Update"),
                              div(class = "card-body",
                                  layout_columns(
                                    dateInput(ns("action_date"), "Date of Update", value = Sys.Date(), width = "100%"),
                                    selectInput(ns("upd_impl_prog"), "Implementation Progress", choices = c("Not specified", "Scheduled for future", "Major issues", "Minor issues", "On-track", "Completed", "Abandoned"), selected = selected_row$Implementation),
                                    selectInput(ns("upd_res_prog"), "Effectiveness Progress", choices = c("Not specified", "Not Yet", "Not achieved", "Partially achieved", "On-track", "Achieved", "No longer relevant"), selected = selected_row$Effectiveness)
                                  ),
                                  div(class = "mt-2", textAreaInput(ns("what_done"), "1. What was done?", rows = 2, width = "100%"), textAreaInput(ns("what_learned"), "2. What was learned?", rows = 2, width = "100%"), textAreaInput(ns("what_needed"), "3. What is still needed?", rows = 2, width = "100%")),
                                  actionButton(ns("submit_progress"), "Submit Update", class = "btn-primary w-100 mt-3", style="font-weight: bold;")
                              )
                          )
                        )
                    )
          ),
          
          # --- TAB 3: SECURE SPATIAL FOOTPRINT ---
          nav_panel("Spatial Footprint", value = "spatial",
                    div(class = "mt-4 card shadow-sm border-0", style = "border-top: 4px solid #EAB11E !important;",
                        div(class = "card-body",
                            h5("Spatial Footprint"),
                            p("View the current spatial boundaries for this action below. To make changes to the scale or boundaries, unlock the map.", class="text-muted"),
                            
                            layout_columns(
                              div(h6(strong("Current Scale: "), textOutput(ns("lbl_current_scale"), inline = TRUE))),
                              div(class = "text-end",
                                  div(class = "form-check form-switch d-inline-block",
                                      tags$input(class = "form-check-input", type = "checkbox", id = ns("map_edit_mode")),
                                      tags$label(class = "form-check-label fw-bold", style="color: #AA5F40;", "Unlock Map for Editing", `for` = ns("map_edit_mode"))
                                  )
                              )
                            ),
                            
                            conditionalPanel(
                              condition = sprintf("input['%s'] == true", ns("map_edit_mode")),
                              div(class = "p-3 mb-3 mt-2 rounded", style = "background-color: #FFF3CD; border: 1px solid #FFEEBA;",
                                  selectInput(ns("upd_spatial_scale"), "Change Scale of Action", choices = allowed_scales, selected = current_scale),
                                  p(class = "text-muted small mb-0", "If 'Custom', use the map tools to draw or edit the boundary. Range maps cannot be modified.")
                              )
                            ),
                            
                            div(class = "border rounded shadow-sm mb-4 mt-2", leafletOutput(ns("upd_action_map"), height = "500px")),
                            
                            conditionalPanel(
                              condition = sprintf("input['%s'] == true", ns("map_edit_mode")),
                              actionButton(ns("submit_spatial"), "Save Spatial Updates", class = "btn-success btn-lg mt-2 w-100", style="font-weight: bold;")
                            )
                        )
                    )
          )
        )
      ))
      
      updateCheckboxInput(session, "map_edit_mode", value = FALSE)
      updateSelectInput(session, "upd_spatial_scale", choices = allowed_scales, selected = current_scale)
      preloaded_geom(current_geom)
      last_drawn_scale(NULL) 
    })
    
    output$lbl_current_scale <- renderText({ input$upd_spatial_scale })
    
    observeEvent(input$btn_close_modal, {
      removeModal()
      selectRows(proxy_action_table, NULL) 
      preloaded_geom(NULL)
      last_drawn_scale(NULL)
    })
    
    # ==========================================
    # MODAL TAB 1: OVERVIEW & THREATS
    # ==========================================
    output$modal_core_details <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      desc <- dbGetQuery(db, "SELECT actiondesc FROM track.implementedactions WHERE implementedactionid = $1", params = list(as.integer(impl_id)))$actiondesc[1]
      
      impl_badge <- get_status_badge(action_data()[input$action_table_rows_selected, "Implementation"])
      eff_badge <- get_status_badge(action_data()[input$action_table_rows_selected, "Effectiveness"])
      
      tagList(
        p(strong("Description: "), if(is.na(desc) || desc=="") "No description provided." else desc),
        p(strong("Current Implementation: "), impl_badge),
        p(strong("Current Effectiveness: "), eff_badge)
      )
    })
    
    output$modal_threats_details <- renderUI({
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      q_threats <- "
        SELECT l2.threatl2code || '. ' || l2.threatl2name AS threat_name, 
               ta.justification, ta.alternative_category, ta.justification_text, 
               CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name, 
               ta.threatl2id 
        FROM track.threatsaddressed ta 
        JOIN track.specieshabitatactions sha ON ta.specieshabitatactionsid = sha.specieshabitatactionsid 
        JOIN proj.l2_threats l2 ON ta.threatl2id = l2.threatl2id 
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid 
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid 
        WHERE sha.implementedactionid = $1
        ORDER BY threat_name ASC, target_name ASC
      "
      threats_df <- dbGetQuery(db, q_threats, params = list(as.integer(impl_id)))
      
      if(nrow(threats_df) > 0) {
        threats_df$threat_name <- trimws(threats_df$threat_name)
        threats_df$group_name <- ifelse(threats_df$threatl2id == 59, paste0("Broader Goal: ", threats_df$alternative_category), threats_df$threat_name)
        unique_groups <- unique(threats_df$group_name)
        
        tags$ul(class = "mt-2 mb-0", lapply(unique_groups, function(g) {
          sub_df <- threats_df[threats_df$group_name == g, ]
          is_broader <- sub_df$threatl2id[1] == 59
          
          threat_title <- if(is_broader) {
            strong("Broader Conservation Goal: ", style="color: #0D67B8;", sub_df$alternative_category[1])
          } else {
            strong(sub_df$threat_name[1])
          }
          
          target_list <- tags$ul(class = "mt-1 mb-3", style = "list-style-type: circle;", lapply(1:nrow(sub_df), function(i) {
            just_text <- if(is_broader) sub_df$justification_text[i] else sub_df$justification[i]
            tags$li(span(class = "text-primary fw-bold", sub_df$target_name[i]), " - ", em("Justification: ", just_text))
          }))
          
          tags$li(class = "mb-2", threat_title, target_list)
        }))
      } else { 
        p(em("No threats recorded.")) 
      }
    })
    
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
      
      collab_list <- tags$ul(class = "mt-2 mb-0", lapply(1:nrow(collabs_df), function(i) {
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
        
        add_tools <- div(class = "mt-3 pt-3 border-top",
                         h6("Delegate a Colleague", class = "fw-bold text-muted mb-2"),
                         selectInput(ns("new_delegate_id"), NULL, choices = c("Choose a user..." = "", user_choices), width = "100%"),
                         actionButton(ns("btn_add_delegate"), "Add Delegate", class = "btn-warning w-100", style = "font-weight: bold;")
        )
        tagList(collab_list, add_tools)
      } else {
        collab_list
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
    
    # ==========================================
    # MODAL TAB 2: PROGRESS LOG
    # ==========================================
    action_history <- reactive({
      req(input$action_table_rows_selected)
      db_sync_trigger() 
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      dbGetQuery(db, "SELECT a.actiondate::date AS \"Date\", a.implementation_progress AS \"Impl. Progress\", a.result_progress AS \"Effectiveness\", a.what_done AS \"What was done?\", COALESCE(p.first_name || ' ' || p.last_name, a.createdby) AS \"Entered By\" FROM track.actiontracking a LEFT JOIN public.profiles p ON a.createdby = p.id::text WHERE a.implementedactionid = $1 ORDER BY a.actiondate DESC", params = list(as.integer(impl_id)))
    })
    
    output$history_table <- renderDT({
      datatable(action_history(), rownames = FALSE, options = list(dom = 't', paging = FALSE, scrollY = "300px", scrollCollapse = TRUE))
    })
    
    observeEvent(input$submit_progress, {
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          dbExecute(conn, "INSERT INTO track.actiontracking (implementedactionid, actiondate, implementation_progress, result_progress, what_done, what_learned, what_needed, createdby) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)", params = list(as.integer(impl_id), as.character(input$action_date), input$upd_impl_prog, input$upd_res_prog, input$what_done, input$what_learned, input$what_needed, current_user()$user_id))
          dbExecute(conn, "UPDATE track.implementedactions SET implementation_progress = $1, result_progress = $2 WHERE implementedactionid = $3", params = list(input$upd_impl_prog, input$upd_res_prog, as.integer(impl_id)))
        })
        showNotification("Progress logged!", type = "message")
        updateTextAreaInput(session, "what_done", value = "")
        updateTextAreaInput(session, "what_learned", value = "")
        updateTextAreaInput(session, "what_needed", value = "")
        db_sync_trigger(db_sync_trigger() + 1)
      }, error = function(e) { showNotification(paste("Error:", e$message), type="error") })
    })
    
    # ==========================================
    # MODAL TAB 3: SPATIAL UPDATES
    # ==========================================
    preloaded_geom <- reactiveVal(NULL)
    
    # OBSERVER: If they unlock the map and switch to "Range", fetch it on the fly so they can save it!
    observeEvent(input$upd_spatial_scale, {
      req(isTRUE(input$map_edit_mode), input$upd_spatial_scale == "Range")
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      id_notif <- showNotification("Re-calculating range maps... Please wait.", type = "message", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id_notif), add = TRUE)
      
      q_sp <- "SELECT DISTINCT speciesid FROM track.specieshabitatactions WHERE implementedactionid = $1 AND specieshabitat = TRUE"
      sp_ids <- dbGetQuery(db, q_sp, params = list(as.integer(impl_id)))$speciesid
      q_hab <- "SELECT DISTINCT habitatsubtypeid FROM track.specieshabitatactions WHERE implementedactionid = $1 AND specieshabitat = FALSE"
      hab_ids <- dbGetQuery(db, q_hab, params = list(as.integer(impl_id)))$habitatsubtypeid
      
      all_geoms <- list()
      if (length(sp_ids[!is.na(sp_ids)]) > 0) {
        q_sp_geom <- sprintf("SELECT ST_AsGeoJSON(ST_Union(ST_MakeValid(geom))) AS geojson FROM proj.species_ranges WHERE speciesid IN (%s)", paste(as.integer(sp_ids[!is.na(sp_ids)]), collapse = ","))
        res_sp <- dbGetQuery(db, q_sp_geom)
        if (nrow(res_sp) > 0 && !is.na(res_sp$geojson[1])) all_geoms <- c(all_geoms, res_sp$geojson[1])
      }
      if (length(hab_ids[!is.na(hab_ids)]) > 0) {
        q_hab_geom <- sprintf("SELECT ST_AsGeoJSON(ST_Union(ST_MakeValid(geom))) AS geojson FROM proj.habitat_ranges WHERE habitatsubtypeid IN (%s)", paste(as.integer(hab_ids[!is.na(hab_ids)]), collapse = ","))
        res_hab <- dbGetQuery(db, q_hab_geom)
        if (nrow(res_hab) > 0 && !is.na(res_hab$geojson[1])) all_geoms <- c(all_geoms, res_hab$geojson[1])
      }
      if (length(all_geoms) > 0) preloaded_geom(all_geoms[[1]]) # Save the calculated range to memory
    }, ignoreInit = TRUE)
    
    # THE MAP RENDERER: Builds the map natively with the geometry already attached!
    output$upd_action_map <- renderLeaflet({
      req(input$modal_tabs == "spatial") 
      
      # FIX: Always lock the view to Colorado!
      m <- leaflet() %>% 
        setView(lng = -105.5, lat = 39.0, zoom = 6) %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addLayersControl(baseGroups = c("Terrain", "Satellite"), options = layersControlOptions(collapsed = FALSE))
      
      geom_json <- preloaded_geom()
      
      # If we have a geometry (Range OR Custom), draw it
      if (!is.null(geom_json) && !is.na(geom_json) && geom_json != "") {
        tryCatch({
          geo_sfc <- sf::st_as_sfc(geom_json, GeoJSON = TRUE)
          geo_sf <- sf::st_sf(geom = geo_sfc, crs = 4326)
          geo_sf <- sf::st_make_valid(geo_sf)
          
          if(nrow(geo_sf) > 0) {
            # Added the polygon, but REMOVED the fitBounds() zoom logic!
            m <- m %>% addPolygons(data = geo_sf, group = "drawn_features", color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 2)
          }
        }, error = function(e) { print(paste("Map Error:", e$message)) })
      }
      
      # If the map is unlocked and set to Custom, add the editing tools to the map builder
      if (isTRUE(input$map_edit_mode) && input$upd_spatial_scale == "Custom") {
        m <- m %>% addDrawToolbar(
          targetGroup = "drawn_features", polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE,
          markerOptions = drawMarkerOptions(), polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions = drawShapeOptions(color = "#EAB11E", fillColor = "#EAB11E", fillOpacity = 0.5, weight = 3)),
          editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
        )
      }
      return(m)
    })
    
    observeEvent(input$submit_spatial, {
      req(input$action_table_rows_selected)
      impl_id <- action_data()[input$action_table_rows_selected, "implementedactionid"]
      
      # Fallback to whatever is currently in memory
      geom_json <- preloaded_geom() 
      
      # Overwrite if they drew a new Custom shape
      if (input$upd_spatial_scale == "Custom" && !is.null(input$upd_action_map_draw_all_features) && length(input$upd_action_map_draw_all_features$features) > 0) {
        geom_json <- as.character(jsonlite::toJSON(input$upd_action_map_draw_all_features$features[[1]]$geometry, auto_unbox = TRUE))
      }
      
      tryCatch({
        dbExecute(db, "DELETE FROM track.action_spatial WHERE implementedactionid = $1", params = list(as.integer(impl_id)))
        
        if (!is.null(geom_json) && !is.na(geom_json) && geom_json != "") {
          dbExecute(db, "INSERT INTO track.action_spatial (implementedactionid, scale_category, geom) VALUES ($1, $2, ST_SetSRID(ST_GeomFromGeoJSON($3), 4326))", params = list(as.integer(impl_id), input$upd_spatial_scale, geom_json))
        } else {
          dbExecute(db, "INSERT INTO track.action_spatial (implementedactionid, scale_category) VALUES ($1, $2)", params = list(as.integer(impl_id), input$upd_spatial_scale))
        }
        
        preloaded_geom(geom_json)
        updateCheckboxInput(session, "map_edit_mode", value = FALSE)
        showNotification("Spatial Footprint Updated!", type = "message")
      }, error = function(e) { showNotification(paste("Error:", e$message), type="error") })
    })
    
  })
}