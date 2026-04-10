admin_ui_users <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card shadow-sm mb-4 mt-3",
        div(class = "card-header text-white fw-bold", style = "background-color: #AA5F40;", "User Management"),
        div(class = "card-body", DTOutput(ns("users_table")))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != null", ns("users_table_rows_selected")),
      div(class = "card shadow-sm mb-5", style = "border-color: #AA5F40;",
          div(class = "card-body",
              h5("Update Role", style = "color: #AA5F40; font-weight: bold;"),
              layout_columns(
                uiOutput(ns("selected_user_info")),
                selectInput(ns("new_role"), "Assign Role:", choices = c("user", "admin")),
                actionButton(ns("btn_save_role"), "Apply Role Change", class = "mt-4 text-white", style = "background-color: #AA5F40; font-weight: bold;")
              )
          )
      )
    )
  )
}

admin_ui_actions <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card shadow-sm mb-4 mt-3",
        div(class = "card-header text-white fw-bold", style = "background-color: #005A53;", "Global Action Directory"),
        div(class = "card-body",
            layout_columns(
              p("Select an action to view details and manage delegates."),
              selectInput(ns("filter_type"), "Filter by Target Type:", choices = c("All", "Species", "Habitat"), width = "100%")
            ),
            DTOutput(ns("actions_table"))
        )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != null", ns("actions_table_rows_selected")),
      div(class = "card shadow-sm mb-5", style = "border-color: #005A53;",
          div(class = "card-header text-white", style = "background-color: #005A53;", "Action Intelligence & Delegation"),
          div(class = "card-body",
              uiOutput(ns("admin_action_details_view"))
          )
      )
    )
  )
}

admin_server <- function(id, db, current_user, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- USER LOGIC ---
    all_users <- reactive({ db_sync_trigger(); dbGetQuery(db, "SELECT id, first_name || ' ' || last_name AS name, email, agency, role FROM public.profiles ORDER BY name") })
    output$users_table <- renderDT({ datatable(all_users(), selection = "single", rownames = FALSE, options = list(pageLength = 5, dom = 'ftp', columnDefs = list(list(visible = FALSE, targets = 0)))) })
    output$selected_user_info <- renderUI({ req(input$users_table_rows_selected); user_row <- all_users()[input$users_table_rows_selected, ]; tagList(p(strong("User: "), user_row$name, br(), strong("Email: "), user_row$email)) })
    observeEvent(input$btn_save_role, { req(input$users_table_rows_selected); user_row <- all_users()[input$users_table_rows_selected, ]; dbExecute(db, "UPDATE public.profiles SET role = $1 WHERE id = $2", params = list(input$new_role, user_row$id)); showNotification("Role updated!", type = "message"); refresh_trigger(db_sync_trigger() + 1) })
    
    # --- ACTION DELEGATION LOGIC ---
    all_actions <- reactive({
      db_sync_trigger()
      query <- "
        SELECT 
          ia.implementedactionid AS \"ID\",
          l2.actionl2name AS \"Action Name\",
          ia.actiondesc AS \"User Description\",
          COALESCE(p.first_name || ' ' || p.last_name, 'No Author Recorded') AS \"Created By\",
          CASE WHEN sha.specieshabitat = TRUE THEN 'Species' ELSE 'Habitat' END AS \"Type\"
        FROM track.implementedactions ia
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN public.profiles p ON ia.createdby = p.id::text
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
      "
      df <- dbGetQuery(db, query)
      if (input$filter_type != "All") df <- df[df$Type == input$filter_type, ]
      df
    })
    
    output$actions_table <- renderDT({
      datatable(all_actions(), selection = "single", rownames = FALSE,
                options = list(pageLength = 5, dom = 'ftp', 
                               columnDefs = list(list(className = 'dt-left', targets = 0))))
    })
    
    output$admin_action_details_view <- renderUI({
      req(input$actions_table_rows_selected)
      action_row <- all_actions()[input$actions_table_rows_selected, ]
      impl_id <- action_row$ID
      
      # 1. Fetch Collaborators
      q_collabs <- "
        SELECT p.first_name || ' ' || p.last_name AS name, 'Creator' AS access_level
        FROM track.implementedactions a JOIN public.profiles p ON a.createdby = p.id::text WHERE a.implementedactionid = $1
        UNION
        SELECT p.first_name || ' ' || p.last_name AS name, 'Delegate' AS access_level
        FROM track.delegateusers d JOIN public.profiles p ON d.userid = p.id::text WHERE d.implementedactionid = $1
      "
      collabs_df <- dbGetQuery(db, q_collabs, params = list(as.integer(impl_id)))
      
      # 2. Fetch Linked Targets (Species/Habitats)
      q_targets <- "
        SELECT CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS target_name
        FROM track.specieshabitatactions sha
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        WHERE sha.implementedactionid = $1
      "
      targets_df <- dbGetQuery(db, q_targets, params = list(as.integer(impl_id)))
      
      # 3. Available Users for Delegation
      q_avail <- "SELECT id, first_name || ' ' || last_name AS name FROM public.profiles WHERE id::text NOT IN 
                  (SELECT COALESCE(createdby, '') FROM track.implementedactions WHERE implementedactionid = $1 UNION SELECT userid FROM track.delegateusers WHERE implementedactionid = $1) ORDER BY name"
      avail_df <- dbGetQuery(db, q_avail, params = list(as.integer(impl_id)))
      user_choices <- setNames(avail_df$id, avail_df$name)
      
      tagList(
        layout_columns(
          div(h6("Action Overview", class="text-muted"), 
              p(strong("ID: "), impl_id, br(), strong("Name: "), action_row$`Action Name`),
              p(strong("Targets: "), paste(targets_df$target_name, collapse = ", "))),
          div(h6("Current Access", class="text-muted"), 
              tags$ul(lapply(1:nrow(collabs_df), function(i) tags$li(collabs_df$name[i], " (", collabs_df$access_level[i], ")"))))
        ),
        hr(),
        h6("Assign New Delegate", class="fw-bold text-muted"),
        layout_columns(
          selectInput(ns("new_delegate_id"), NULL, choices = c("Choose a user..." = "", user_choices)),
          actionButton(ns("btn_add_delegate"), "Assign to Action", class = "btn-success w-100")
        )
      )
    })
    
    observeEvent(input$btn_add_delegate, {
      req(input$actions_table_rows_selected, input$new_delegate_id)
      dbExecute(db, "INSERT INTO track.delegateusers (implementedactionid, userid) VALUES ($1, $2)", 
                params = list(as.integer(all_actions()$ID[input$actions_table_rows_selected]), input$new_delegate_id))
      showNotification("Delegate added!", type = "message")
      refresh_trigger(db_sync_trigger() + 1)
    })
  })
}