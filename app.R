library(shiny)
library(bslib)
library(dplyr)
library(DBI)
library(DT)

# Source all helpers and modules
source("R/db_connections.R")
source("R/dashboard.R")
source("R/profile.R")
source("R/add_action.R")
source("R/update_action.R")

cpw_theme <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = "#212529", primary = "#0D67B8", 
  secondary = "#ECE8E4", success = "#055A53", warning = "#EAB11E", danger = "#AA5F40", 
  base_font = font_google("Lato"), heading_font = font_google("Lato")
) |>
  bs_add_rules(".navbar { background-color: #07234C !important; } .navbar-brand, .nav-link { color: #FFFFFF !important; }")

ui <- page_navbar(
  id = "main_nav", 
  theme = cpw_theme,
  title = div(img(src = "cpw_logo.png", height = "40px", style = "margin-right: 10px; margin-top: -5px;"), "SWAP Action Tracker"),
  
  # Inject our custom CSS for the sidebar and the dynamic active highlighter
  header = tagList(
    uiOutput("nav_styles"),
    tags$style(HTML("
      .sidebar-btn {
        width: 100%; text-align: left; background: transparent !important; border: none !important;
        color: #212529 !important; padding: 12px 15px; border-radius: 6px; font-weight: 600;
        margin-bottom: 5px; box-shadow: none !important; font-size: 1rem; transition: 0.2s;
      }
      .sidebar-btn:hover { background-color: #ECE8E4 !important; color: #07234C !important; }
    "))
  ),
  
  # Global Sidebar
  sidebar = sidebar(
    open = "open", 
    title = p("Menu", style = "font-weight: bold; color: #07234C; font-size: 1.2rem; margin-bottom: 15px; border-bottom: 2px solid #ECE8E4; padding-bottom: 10px;"),
    bg = "#FAFAFA",
    
    # We replaced the bulky button classes with our clean custom 'sidebar-btn'
    actionButton("nav_dash", "Dashboard Home", class = "sidebar-btn"),
    actionButton("nav_add", "Report New Action", class = "sidebar-btn"),
    actionButton("nav_update", "Update Existing Action", class = "sidebar-btn"),
    actionButton("nav_profile", "My Profile", class = "sidebar-btn")
  ),
  
  nav_panel("Dashboard", value = "dashboard", dashboard_ui("dashboard")),
  nav_panel_hidden(value = "add_action", add_action_ui("add_action")),
  nav_panel_hidden(value = "update_action", update_action_ui("update_action")),
  nav_panel_hidden(value = "profile", profile_ui("profile"))
)

server <- function(input, output, session) {
  # 1. Dummy Auth State 
  current_user <- reactiveVal(list(UserID = 1, Role = "Admin", FirstName = "Jane", LastName = "Doe"))
  
  # 2. Database Connection
  db <- connect_supabase()
  onStop(function() { dbDisconnect(db) })
  
  # 3. Initialize Modules
  dashboard_server("dashboard", db)
  profile_server("profile", db, current_user)
  add_triggers <- add_action_server("add_action", db, current_user)
  update_action_server("update_action", db, current_user)
  
  # 4. Dynamic Sidebar Highlighting CSS
  # This listens to what tab you are on and injects the dark blue color onto the matching button
  output$nav_styles <- renderUI({
    active_btn <- switch(input$main_nav,
                         "dashboard" = "#nav_dash",
                         "add_action" = "#nav_add",
                         "update_action" = "#nav_update",
                         "profile" = "#nav_profile"
    )
    
    tags$style(HTML(paste0("
      ", active_btn, " {
        background-color: #07234C !important;
        color: white !important;
      }
    ")))
  })
  
  # 5. Cross-Navigation Logic
  observeEvent(input$nav_dash | add_triggers$go_home(), {
    nav_select("main_nav", "dashboard")
  }, ignoreInit = TRUE)
  
  observeEvent(input$nav_add, {
    nav_select("main_nav", "add_action")
  }, ignoreInit = TRUE)
  
  observeEvent(input$nav_update, {
    nav_select("main_nav", "update_action")
  }, ignoreInit = TRUE)
  
  observeEvent(input$nav_profile, {
    nav_select("main_nav", "profile")
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)