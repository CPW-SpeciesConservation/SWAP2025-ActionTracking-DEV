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
    
    # --- APPROACH SELECTION ---
    div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
        div(class = "card-header bg-primary text-white", "Recording Approach"),
        div(class = "card-body", style = "overflow: visible;",
            radioButtons(ns("wizard_path"), "How would you like to record this action?",
                         choices = c("Species/Habitat oriented" = "species", "Action oriented" = "action"),
                         selected = character(0), inline = TRUE)
        )
    ),
    
    # ==========================================
    # PATH A: SPECIES/HABITAT ORIENTED WIZARD
    # ==========================================
    conditionalPanel(
      condition = sprintf("input['%s'] == 'species'", ns("wizard_path")),
      
      div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
          div(class = "card-header bg-secondary text-white", "Target Selection"),
          div(class = "card-body", style = "overflow: visible;",
              radioButtons(ns("target_type"), "Target Type:", choices = c("Species", "Habitat"), inline = TRUE),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Species'", ns("target_type")),
                layout_columns(
                  selectInput(ns("tax_group"), "Taxonomic Group", choices = c("Loading..." = "")),
                  selectizeInput(ns("species_select"), "Species", choices = c("Select a group first..." = ""), options = list(dropdownParent = "body"))
                ),
                uiOutput(ns("species_info_card"))
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
                  selectInput(ns("timeframe"), "Timeframe Filter", choices = c("Short Term", "Long Term")),
                  selectInput(ns("status"), "Status", choices = c("Planned", "In Progress", "Completed"))
                ),
                layout_columns(
                  selectInput(ns("action_l2"), "Select L2 Action", choices = c("Loading..." = "")),
                  selectizeInput(ns("action_detail"), "Action Detail (Optional)", choices = c("None" = "none"), options = list(dropdownParent = "body"))
                ),
                hr(),
                h5("Project Summary"),
                textAreaInput(ns("action_desc"), "General Action Description", rows = 2),
                hr(),
                h5("Threats Addressed"),
                p("Select the threats this action mitigates. A justification box will appear for each threat you select:"),
                
                uiOutput(ns("dynamic_threats_ui_path_a")),
                
                actionButton(ns("btn_submit_species"), "Submit Action to Database", class = "btn-success btn-lg mt-4 w-100")
            )
        )
      )
    ),
    
    # ==========================================
    # PATH B: ACTION ORIENTED WIZARD
    # ==========================================
    conditionalPanel(
      condition = sprintf("input['%s'] == 'action'", ns("wizard_path")),
      
      div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
          div(class = "card-header bg-secondary text-white", "Action Definition"),
          div(class = "card-body", style = "overflow: visible;",
              p("Use the Action Lexicon below as a guide to find your exact L0, L1, and L2 actions."),
              div(class = "text-center mb-4", tags$img(src = "action_lexicon.png", class = "img-fluid border rounded shadow-sm", style = "max-height: 400px; width: auto;")),
              layout_columns(
                selectInput(ns("act_timeframe"), "Timeframe", choices = c("Short Term", "Long Term")),
                selectInput(ns("act_status"), "Status", choices = c("Planned", "In Progress", "Completed"))
              ),
              layout_columns(
                selectInput(ns("act_l0"), "Level 0 Category", choices = c("Loading..." = "")),
                selectInput(ns("act_l1"), "Level 1 Category", choices = c("Select L0 first..." = ""))
              ),
              selectInput(ns("act_l2"), "Level 2 Action", choices = c("Select L1 first..." = "")),
              textAreaInput(ns("act_desc"), "General Action Description", rows = 2)
          )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] != ''", ns("act_l2")),
        div(class = "card mb-4 shadow-sm", style = "overflow: visible;",
            div(class = "card-header bg-primary text-white", "Target Selection & Mitigated Threats"),
            div(class = "card-body", style = "overflow: visible;",
                p(strong("Based on your selected Action and Timeframe, select all applicable Species and Habitats this project targets. Then, assign the specific threats it mitigates for each target.")),
                layout_columns(
                  selectizeInput(ns("act_target_species"), "Target Species (Filtered)", choices = NULL, multiple = TRUE, options = list(placeholder = "Select species...", dropdownParent = "body")),
                  selectizeInput(ns("act_target_habitats"), "Target Habitats (Filtered)", choices = NULL, multiple = TRUE, options = list(placeholder = "Select habitats...", dropdownParent = "body"))
                ),
                hr(),
                uiOutput(ns("dynamic_target_threats_ui_path_b")),
                actionButton(ns("btn_submit_action"), "Submit Multi-Target Action", class = "btn-success btn-lg mt-4 w-100")
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
    
    # ==========================================
    # SHARED DROPDOWNS & INITIALIZATION
    # ==========================================
    observe({
      tax_groups <- dbGetQuery(db, "SELECT groupname FROM proj.taxonomicgroups ORDER BY groupname")
      updateSelectInput(session, "tax_group", choices = c("Choose a group..." = "", tax_groups$groupname))
      habitats <- dbGetQuery(db, "SELECT majorhabitatname FROM proj.majorhabitats ORDER BY majorhabitatname")
      updateSelectInput(session, "major_habitat", choices = c("Choose a major habitat..." = "", habitats$majorhabitatname))
      l0_acts <- dbGetQuery(db, "SELECT actionl0id, actionl0name FROM proj.l0_actions ORDER BY actionl0name")
      updateSelectInput(session, "act_l0", choices = c("Choose L0 Category..." = "", setNames(l0_acts$actionl0id, l0_acts$actionl0name)))
    })
    
    # ==========================================
    # PATH A: SPECIES/HABITAT SERVER LOGIC
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
         FROM xref.species_threatsl2 x 
         JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id 
         JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id
         JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id
         WHERE x.speciesid = $1 
         ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)"
      } else {
        "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context
         FROM xref.habitat_threatsl2 x 
         JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id 
         JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id
         JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id
         WHERE x.habitatsubtypeid = $1 
         ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)"
      }
      valid_threats_df_a(dbGetQuery(db, q_threats, params = list(target_id)))
    })
    
    output$dynamic_threats_ui_path_a <- renderUI({
      vt <- valid_threats_df_a()
      if (nrow(vt) == 0) return(p(em("No threats mapped to this target.")))
      
      l0_groups <- unique(vt$l0_context)
      lapply(l0_groups, function(l0) {
        sub_l0 <- vt[vt$l0_context == l0, ]
        div(class = "mb-4",
            h5(l0, style="color: #055A53; font-weight: bold; border-bottom: 3px solid #07234C; padding-bottom: 5px; margin-top: 20px;"),
            
            lapply(unique(sub_l0$l1_context), function(l1) {
              sub_l1 <- sub_l0[sub_l0$l1_context == l1, ]
              div(class = "mb-3 ms-4",
                  h6(l1, style="color: #07234C; font-weight: bold; border-bottom: 1px solid #CCCCCC; padding-bottom: 3px; font-size: 1.05em; margin-top: 10px;"),
                  
                  lapply(1:nrow(sub_l1), function(i) {
                    t_id <- sub_l1$threatl2id[i]
                    chk_id <- paste0("chk_threat_a_", t_id)
                    txt_id <- paste0("just_threat_a_", t_id)
                    tagList(
                      checkboxInput(session$ns(chk_id), sub_l1$threat_name[i]),
                      conditionalPanel(
                        condition = sprintf("input['%s'] == true", session$ns(chk_id)),
                        div(class = "ps-4 mb-3", textAreaInput(session$ns(txt_id), "Justification:", rows = 1))
                      )
                    )
                  })
              )
            })
        )
      })
    })
    
    observeEvent(input$btn_submit_species, {
      vt <- valid_threats_df_a()
      selected_threats <- c()
      for(t_id in vt$threatl2id) { if(isTRUE(input[[paste0("chk_threat_a_", t_id)]])) selected_threats <- c(selected_threats, t_id) }
      
      if (input$action_l2 == "" || length(selected_threats) == 0) {
        showNotification("Please select an Action and at least one Threat.", type = "error")
        return()
      }
      
      for (t_id in selected_threats) {
        if (is.null(input[[paste0("just_threat_a_", t_id)]]) || trimws(input[[paste0("just_threat_a_", t_id)]]) == "") {
          showNotification("Please provide a justification for all selected threats.", type = "error")
          return()
        }
      }
      
      # THE FIX: Using poolWithTransaction for proper execution and rollback safety
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          is_species <- input$target_type == "Species"
          target_id <- as.integer(if(is_species) input$species_select else input$habitat_subtype)
          detail_val <- if(is.null(input$action_detail) || input$action_detail == "none" || input$action_detail == "") NA_integer_ else as.integer(input$action_detail)
          
          q1 <- "INSERT INTO track.implementedactions (actionl2id, timeframe, actiondesc, status, createdby) VALUES ($1, $2, $3, $4, $5) RETURNING implementedactionid"
          res1 <- dbGetQuery(conn, q1, params = list(as.integer(input$action_l2), input$timeframe, input$action_desc, input$status, current_user()$user_id))
          new_impl_id <- res1$implementedactionid[1]
          
          if (is_species) {
            q2 <- "INSERT INTO track.specieshabitatactions (specieshabitat, speciesid, speciesactiondetailid, implementedactionid, createdby) VALUES (TRUE, $1, $2, $3, $4) RETURNING specieshabitatactionsid"
          } else {
            q2 <- "INSERT INTO track.specieshabitatactions (specieshabitat, habitatsubtypeid, habitatactiondetailid, implementedactionid, createdby) VALUES (FALSE, $1, $2, $3, $4) RETURNING specieshabitatactionsid"
          }
          res2 <- dbGetQuery(conn, q2, params = list(target_id, detail_val, new_impl_id, current_user()$user_id))
          new_sha_id <- res2$specieshabitatactionsid[1]
          
          q3 <- "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, createdby, justification) VALUES ($1, $2, $3, $4)"
          for (t_id in selected_threats) {
            dbExecute(conn, q3, params = list(new_sha_id, as.integer(t_id), current_user()$user_id, input[[paste0("just_threat_a_", t_id)]]))
          }
        })
        
        # Code down here only executes if the transaction completes successfully
        db_sync_trigger(db_sync_trigger() + 1)
        showNotification("Success! Action recorded to database.", type = "message", duration = 5)
        
        updateTextAreaInput(session, "action_desc", value = "")
        for(t_id in vt$threatl2id) {
          updateCheckboxInput(session, paste0("chk_threat_a_", t_id), value = FALSE)
          updateTextAreaInput(session, paste0("just_threat_a_", t_id), value = "")
        }
        nav_home_trigger(nav_home_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    })
    
    # ==========================================
    # PATH B: ACTION-ORIENTED SERVER LOGIC
    # ==========================================
    observeEvent(input$act_l0, {
      req(input$act_l0)
      q <- "SELECT actionl1id, actionl1code || '. ' || actionl1name AS n FROM proj.l1_actions WHERE actionl0id = $1"
      res <- dbGetQuery(db, q, params = list(input$act_l0))
      updateSelectInput(session, "act_l1", choices = c("Select L1 Category..." = "", setNames(res$actionl1id, res$n)))
    })
    
    observeEvent(input$act_l1, {
      req(input$act_l1)
      q <- "SELECT actionl2id, actionl2code || '. ' || actionl2name AS n FROM proj.l2_actions WHERE actionl1id = $1"
      res <- dbGetQuery(db, q, params = list(input$act_l1))
      updateSelectInput(session, "act_l2", choices = c("Select L2 Action..." = "", setNames(res$actionl2id, res$n)))
    })
    
    observeEvent(list(input$act_l2, input$act_timeframe), {
      req(input$act_l2, input$act_timeframe)
      
      q_sp <- "SELECT s.speciesid, s.commonname, tg.groupname FROM xref.species_actionsl2 x JOIN proj.species s ON x.speciesid = s.speciesid JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid WHERE x.actionl2id = $1 AND TRIM(LOWER(x.timeframe)) = TRIM(LOWER($2)) AND s.swap_rankingid IN (1, 2) ORDER BY tg.groupname, s.commonname"
      res_sp <- dbGetQuery(db, q_sp, params = list(input$act_l2, input$act_timeframe))
      if (nrow(res_sp) > 0) {
        updateSelectizeInput(session, "act_target_species", choices = split(setNames(res_sp$speciesid, res_sp$commonname), res_sp$groupname), options = list(placeholder = "Select species...", dropdownParent = "body"))
      } else { updateSelectizeInput(session, "act_target_species", choices = character(0), options = list(placeholder = "No species mapped to this action.", dropdownParent = "body")) }
      
      q_hab <- "SELECT hs.habitatsubtypeid, hs.habitatsubtypename, mh.majorhabitatname FROM xref.habitat_actionsl2 x JOIN proj.habitatsubtypes hs ON x.habitatsubtypeid = hs.habitatsubtypeid JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid WHERE x.actionl2id = $1 AND TRIM(LOWER(x.timeframe)) = TRIM(LOWER($2)) ORDER BY mh.majorhabitatname, hs.habitatsubtypename"
      res_hab <- dbGetQuery(db, q_hab, params = list(input$act_l2, input$act_timeframe))
      if (nrow(res_hab) > 0) {
        updateSelectizeInput(session, "act_target_habitats", choices = split(setNames(res_hab$habitatsubtypeid, res_hab$habitatsubtypename), res_hab$majorhabitatname), options = list(placeholder = "Select habitats...", dropdownParent = "body"))
      } else { updateSelectizeInput(session, "act_target_habitats", choices = character(0), options = list(placeholder = "No habitats mapped to this action.", dropdownParent = "body")) }
    })
    
    output$dynamic_target_threats_ui_path_b <- renderUI({
      sel_sp <- input$act_target_species
      sel_hab <- input$act_target_habitats
      if (length(sel_sp) == 0 && length(sel_hab) == 0) return(p("Please select at least one species or habitat above."))
      
      ui_blocks <- list()
      
      if (length(sel_sp) > 0) {
        for (sp_id in sel_sp) {
          sp_name <- dbGetQuery(db, "SELECT commonname FROM proj.species WHERE speciesid = $1", params = list(sp_id))$commonname[1]
          threats <- dbGetQuery(db, "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context FROM xref.species_threatsl2 x JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id WHERE x.speciesid = $1 ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)", params = list(sp_id))
          
          ui_blocks[[length(ui_blocks) + 1]] <- div(class = "target-card",
                                                    h5(sp_name, class = "text-primary fw-bold mb-3"),
                                                    if(nrow(threats) > 0) {
                                                      tagList(
                                                        p("Select mitigated threats for this species:"),
                                                        lapply(unique(threats$l0_context), function(l0) {
                                                          sub_l0 <- threats[threats$l0_context == l0, ]
                                                          div(class = "mb-3",
                                                              h6(l0, style="color: #055A53; font-weight: bold; border-bottom: 2px solid #07234C; padding-bottom: 4px; margin-top: 15px;"),
                                                              lapply(unique(sub_l0$l1_context), function(l1) {
                                                                sub_l1 <- sub_l0[sub_l0$l1_context == l1, ]
                                                                div(class = "mb-2 ms-4",
                                                                    h6(l1, style="color: #07234C; font-weight: bold; border-bottom: 1px solid #CCCCCC; padding-bottom: 3px; font-size: 1.05em; margin-top: 10px;"),
                                                                    lapply(1:nrow(sub_l1), function(i) {
                                                                      t_id <- sub_l1$threatl2id[i]
                                                                      chk_id <- paste0("act_chk_sp_", sp_id, "_", t_id)
                                                                      txt_id <- paste0("act_txt_sp_", sp_id, "_", t_id)
                                                                      tagList(
                                                                        checkboxInput(session$ns(chk_id), sub_l1$threat_name[i]),
                                                                        conditionalPanel(
                                                                          condition = sprintf("input['%s'] == true", session$ns(chk_id)),
                                                                          div(class = "ps-4 mb-2", textAreaInput(session$ns(txt_id), "Justification:", rows = 1))
                                                                        )
                                                                      )
                                                                    })
                                                                )
                                                              })
                                                          )
                                                        })
                                                      )
                                                    } else { p(em("No threats mapped to this species.")) }
          )
        }
      }
      
      if (length(sel_hab) > 0) {
        for (hab_id in sel_hab) {
          hab_name <- dbGetQuery(db, "SELECT habitatsubtypename FROM proj.habitatsubtypes WHERE habitatsubtypeid = $1", params = list(hab_id))$habitatsubtypename[1]
          threats <- dbGetQuery(db, "SELECT l2.threatl2id, l2.threatl2code || '. ' || l2.threatl2name AS threat_name, l1.threatl1code || '. ' || l1.threatl1name AS l1_context, l0.threatl0name AS l0_context FROM xref.habitat_threatsl2 x JOIN proj.l2_threats l2 ON x.threatl2id = l2.threatl2id JOIN proj.l1_threats l1 ON l2.threatl1id = l1.threatl1id JOIN proj.l0_threats l0 ON l1.threatl0id = l0.threatl0id WHERE x.habitatsubtypeid = $1 ORDER BY l0.threatl0code, CAST(SPLIT_PART(l2.threatl2code, '.', 1) AS INTEGER), CAST(SPLIT_PART(l2.threatl2code, '.', 2) AS INTEGER)", params = list(hab_id))
          
          ui_blocks[[length(ui_blocks) + 1]] <- div(class = "target-card",
                                                    h5(hab_name, class = "text-success fw-bold mb-3"),
                                                    if(nrow(threats) > 0) {
                                                      tagList(
                                                        p("Select mitigated threats for this habitat:"),
                                                        lapply(unique(threats$l0_context), function(l0) {
                                                          sub_l0 <- threats[threats$l0_context == l0, ]
                                                          div(class = "mb-3",
                                                              h6(l0, style="color: #055A53; font-weight: bold; border-bottom: 2px solid #07234C; padding-bottom: 4px; margin-top: 15px;"),
                                                              lapply(unique(sub_l0$l1_context), function(l1) {
                                                                sub_l1 <- sub_l0[sub_l0$l1_context == l1, ]
                                                                div(class = "mb-2 ms-4",
                                                                    h6(l1, style="color: #07234C; font-weight: bold; border-bottom: 1px solid #CCCCCC; padding-bottom: 3px; font-size: 1.05em; margin-top: 10px;"),
                                                                    lapply(1:nrow(sub_l1), function(i) {
                                                                      t_id <- sub_l1$threatl2id[i]
                                                                      chk_id <- paste0("act_chk_hab_", hab_id, "_", t_id)
                                                                      txt_id <- paste0("act_txt_hab_", hab_id, "_", t_id)
                                                                      tagList(
                                                                        checkboxInput(session$ns(chk_id), sub_l1$threat_name[i]),
                                                                        conditionalPanel(
                                                                          condition = sprintf("input['%s'] == true", session$ns(chk_id)),
                                                                          div(class = "ps-4 mb-2", textAreaInput(session$ns(txt_id), "Justification:", rows = 1))
                                                                        )
                                                                      )
                                                                    })
                                                                )
                                                              })
                                                          )
                                                        })
                                                      )
                                                    } else { p(em("No threats mapped to this habitat.")) }
          )
        }
      }
      return(tagList(ui_blocks))
    })
    
    observeEvent(input$btn_submit_action, {
      sel_sp <- input$act_target_species
      sel_hab <- input$act_target_habitats
      if (length(sel_sp) == 0 && length(sel_hab) == 0) {
        showNotification("Please select at least one target.", type = "error")
        return()
      }
      
      # THE FIX: Using poolWithTransaction for proper execution and rollback safety
      tryCatch({
        pool::poolWithTransaction(db, function(conn) {
          
          q1 <- "INSERT INTO track.implementedactions (actionl2id, timeframe, actiondesc, status, createdby) VALUES ($1, $2, $3, $4, $5) RETURNING implementedactionid"
          res1 <- dbGetQuery(conn, q1, params = list(as.integer(input$act_l2), input$act_timeframe, input$act_desc, input$act_status, current_user()$user_id))
          new_impl_id <- res1$implementedactionid[1]
          
          if (length(sel_sp) > 0) {
            for (sp_id in sel_sp) {
              q2 <- "INSERT INTO track.specieshabitatactions (specieshabitat, speciesid, implementedactionid, createdby) VALUES (TRUE, $1, $2, $3) RETURNING specieshabitatactionsid"
              res2 <- dbGetQuery(conn, q2, params = list(sp_id, new_impl_id, current_user()$user_id))
              new_sha_id <- res2$specieshabitatactionsid[1]
              
              threats <- dbGetQuery(conn, "SELECT threatl2id FROM xref.species_threatsl2 WHERE speciesid = $1", params = list(sp_id))
              for (t_id in threats$threatl2id) {
                chk_val <- input[[paste0("act_chk_sp_", sp_id, "_", t_id)]]
                if (!is.null(chk_val) && chk_val == TRUE) {
                  txt_val <- input[[paste0("act_txt_sp_", sp_id, "_", t_id)]]
                  dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, createdby, justification) VALUES ($1, $2, $3, $4)", params = list(new_sha_id, t_id, current_user()$user_id, txt_val))
                }
              }
            }
          }
          
          if (length(sel_hab) > 0) {
            for (hab_id in sel_hab) {
              q2 <- "INSERT INTO track.specieshabitatactions (specieshabitat, habitatsubtypeid, implementedactionid, createdby) VALUES (FALSE, $1, $2, $3) RETURNING specieshabitatactionsid"
              res2 <- dbGetQuery(conn, q2, params = list(hab_id, new_impl_id, current_user()$user_id))
              new_sha_id <- res2$specieshabitatactionsid[1]
              
              threats <- dbGetQuery(conn, "SELECT threatl2id FROM xref.habitat_threatsl2 WHERE habitatsubtypeid = $1", params = list(hab_id))
              for (t_id in threats$threatl2id) {
                chk_val <- input[[paste0("act_chk_hab_", hab_id, "_", t_id)]]
                if (!is.null(chk_val) && chk_val == TRUE) {
                  txt_val <- input[[paste0("act_txt_hab_", hab_id, "_", t_id)]]
                  dbExecute(conn, "INSERT INTO track.threatsaddressed (specieshabitatactionsid, threatl2id, createdby, justification) VALUES ($1, $2, $3, $4)", params = list(new_sha_id, t_id, current_user()$user_id, txt_val))
                }
              }
            }
          }
        })
        
        # Code down here only executes if the transaction completes successfully
        db_sync_trigger(db_sync_trigger() + 1)
        showNotification("Success! Multi-target action recorded.", type = "message", duration = 5)
        updateTextAreaInput(session, "act_desc", value = "")
        updateSelectizeInput(session, "act_target_species", selected = character(0))
        updateSelectizeInput(session, "act_target_habitats", selected = character(0))
        nav_home_trigger(nav_home_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
      })
    })
    
    return(list(go_home = nav_home_trigger))
  })
}