home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container mt-5 text-center",
        h1("Welcome to the CPW SWAP Action Tracker", style = "color: #07234C; font-weight: bold;"),
        p(class = "lead mb-5", "Track, update, and manage conservation actions across Colorado."),
        
        layout_columns(
          card(
            card_header("Get Started", class = "bg-primary text-white"),
            p("Log a brand new conservation action into the database."),
            actionButton(ns("btn_add"), "Report New Action", class = "btn-primary btn-lg mt-3")
          ),
          card(
            card_header("Manage Actions", class = "bg-secondary"),
            p("Add survey results or completion percentages to your existing actions."),
            actionButton(ns("btn_update"), "Update Existing Action", class = "btn-secondary btn-lg mt-3")
          )
        )
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return the button clicks as reactives to the main app.R
    return(list(
      go_add = reactive(input$btn_add),
      go_update = reactive(input$btn_update)
    ))
  })
}