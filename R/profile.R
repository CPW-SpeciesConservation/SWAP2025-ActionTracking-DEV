profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("User Profile & Delegations", class = "mb-4"),
    layout_columns(
      card(
        card_header("My Information", class = "bg-secondary"),
        textInput(ns("first_name"), "First Name"),
        textInput(ns("last_name"), "Last Name"),
        textInput(ns("job_title"), "Job Title"),
        textInput(ns("agency"), "Agency (Locked)", value = "CPW"),
        textInput(ns("role"), "Role (Locked)", value = "Admin"),
        actionButton(ns("save_profile"), "Save Changes", class = "btn-primary")
      ),
      card(
        card_header("My Actions & Delegations", class = "bg-primary text-white"),
        p("Actions you have created or are delegated to manage:"),
        DTOutput(ns("delegation_table"))
      )
    )
  )
}

profile_server <- function(id, db, current_user) {
  moduleServer(id, function(input, output, session) {
    
    # Pre-fill user data based on current_user
    observe({
      updateTextInput(session, "first_name", value = current_user()$FirstName)
      updateTextInput(session, "last_name", value = current_user()$LastName)
      
      # Quick trick to make locked fields readonly without needing extra packages
      shiny::updateTextInput(session, "agency", value = "CPW")
      shiny::updateTextInput(session, "role", value = current_user()$Role)
    })
    
    # Fetch real user actions from the database
    user_actions <- reactive({
      query <- "
        SELECT DISTINCT
          CASE WHEN sha.specieshabitat = TRUE THEN s.commonname ELSE hs.habitatsubtypename END AS \"Target\",
          l2.actionl2name AS \"Action\",
          CASE WHEN ia.createdby = $1 THEN 'Creator' ELSE 'Delegate' END AS \"Role\"
        FROM track.implementedactions ia
        LEFT JOIN track.specieshabitatactions sha ON ia.implementedactionid = sha.implementedactionid
        LEFT JOIN proj.l2_actions l2 ON ia.actionl2id = l2.actionl2id
        LEFT JOIN proj.species s ON sha.speciesid = s.speciesid
        LEFT JOIN proj.habitatsubtypes hs ON sha.habitatsubtypeid = hs.habitatsubtypeid
        LEFT JOIN track.delegateusers du ON ia.implementedactionid = du.implementedactionid
        WHERE ia.createdby = $1 OR du.userid = $2
        ORDER BY \"Target\"
      "
      dbGetQuery(db, query, params = list(current_user()$FirstName, current_user()$UserID))
    })
    
    output$delegation_table <- renderDT({
      datatable(user_actions(), rownames = FALSE, options = list(pageLength = 8, dom = 'tp'))
    })
  })
}