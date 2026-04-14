library(shiny)
library(bslib)
library(dplyr)
library(DBI)
library(DT)
library(shinyjs)
library(memoise)
library(cachem)
library(ggplot2)
library(plotly)
library(networkD3)
library(visNetwork)
library(igraph)
library(ggraph)
library(heatmaply)

# Source all helpers and modules
source("R/db_connections.R")
source("R/dashboard.R")
source("R/profile.R")
source("R/add_action.R")
source("R/update_action.R")
source("R/auth.R")
source("R/admin.R")

# Cleaned up theme! We removed the inline bs_add_rules because your custom_styles.css handles it now.
cpw_theme <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = "#212529", primary = "#0D67B8", 
  secondary = "#ECE8E4", success = "#055A53", warning = "#EAB11E", danger = "#AA5F40", 
  base_font = font_google("Lato"), heading_font = font_google("Lato")
)

ui <- page_navbar(
  id = "main_nav", 
  theme = cpw_theme,
  title = div(img(src = "cpw_logo.png", height = "40px", style = "margin-right: 10px; margin-top: -5px;"), "SWAP Action Tracker"),
  
  # Inject our custom CSS file, plus the dynamic active highlighter
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")
    ),
    uiOutput("nav_styles"),
    tags$style(HTML("
      .sidebar-btn {
        width: 100%; text-align: left; background: transparent !important; border: none !important;
        color: #212529 !important; padding: 12px 15px; border-radius: 6px; font-weight: 900;
        margin-bottom: 5px; box-shadow: none !important; font-size: 1rem; transition: 0.2s;
      }
      .sidebar-btn:hover { background-color: #ECE8E4 !important; color: #07234C !important; }
    "))
  ),
  
  # Global Sidebar (Dynamic based on login state)
  sidebar = sidebar(
    open = "closed", 
    title = p("Menu", style = "font-weight: bold; color: #07234C; font-size: 1.2rem; margin-bottom: 15px; border-bottom: 2px solid #ECE8E4; padding-bottom: 10px;"),
    bg = "#FAFAFA",
    
    # Let the server decide which buttons to show!
    uiOutput("dynamic_sidebar")
  ),
  
  nav_panel("Dashboard", value = "dashboard", dashboard_ui("dashboard")),
  nav_panel_hidden(value = "login", auth_ui("auth")), 
  nav_panel_hidden(value = "add_action", add_action_ui("add_action")),
  nav_panel_hidden(value = "update_action", update_action_ui("update_action")), 
  nav_panel_hidden(value = "profile", profile_ui("profile"))
)

server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)
  
  # Session Timeout Watchdog
  observe({
    req(current_user()) # Only run this if someone is actually logged in
    
    # Wake this code block up every 60 seconds (60000 milliseconds)
    invalidateLater(60000) 
    
    # Check if the current time has passed the expiration time
    if (Sys.time() > current_user()$expires_at) {
      current_user(NULL) # Forcefully erase the user data (logs them out)
      showNotification("Your secure session has expired. Please log in again.", type = "warning", duration = NULL)
    }
  })
  
  # Global Database Sync Trigger
  db_sync_trigger <- reactiveVal(0)
  
  observeEvent(db_sync_trigger(), {
    if (db_sync_trigger() > 0) {
      global_db_cache$reset() 
    }
  }, ignoreInit = TRUE)
  
  # Database Connection
  db <- connect_supabase()
  onStop(function() { pool::poolClose(db) })
  
  # 3. Initialize Modules (Pass the trigger to the modules that need it)
  dashboard_server("dashboard", db, db_sync_trigger)
  auth_server("auth", db, current_user) 
  profile_triggers <- profile_server("profile", db, current_user, db_sync_trigger)
  add_triggers <- add_action_server("add_action", db, current_user, db_sync_trigger)
  update_action_server("update_action", db, current_user, db_sync_trigger)
  
  output$dynamic_sidebar <- renderUI({
    if (is.null(current_user())) {
      tagList(
        actionButton("nav_dash", "Dashboard", class = "sidebar-btn"),
        actionButton("nav_login", "Login / Register", class = "sidebar-btn")
      )
    } else {
      tagList(
        actionButton("nav_dash", "Dashboard", class = "sidebar-btn"),
        actionButton("nav_add", "Report New Action", class = "sidebar-btn"),
        actionButton("nav_update", "Update Existing Action", class = "sidebar-btn"),
        actionButton("nav_profile", "My Profile", class = "sidebar-btn")
      )
    }
  })
  
  # 4. Dynamic Sidebar Highlighting CSS
  output$nav_styles <- renderUI({
    active_btn <- switch(input$main_nav,
                         "dashboard" = "#nav_dash",
                         "login"="#nav_login",
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
  
  observeEvent(input$nav_dash, { nav_select("main_nav", "dashboard") })
  
  observe({
    req(add_triggers)
    observeEvent(add_triggers$go_home(), {
      nav_select("main_nav", "dashboard")
    })
  })
  
  observeEvent(current_user(), {
    if (!is.null(current_user())) {
      # On Login: Jump to Dashboard
      nav_select("main_nav", "dashboard")
    } else {
      # On Logout: Jump to Login
      nav_select("main_nav", "login")
    }
  }) 
  
  observeEvent(input$nav_login, { nav_select("main_nav", "login") })
  
  observeEvent(input$nav_add, {
    req(current_user())
    nav_select("main_nav", "add_action")
  })
  
  observeEvent(input$nav_update, {
    req(current_user())
    nav_select("main_nav", "update_action")
  })
  
  observeEvent(input$nav_profile, {
    req(current_user())
    nav_select("main_nav", "profile")
  })
  
  # Listen for the logout trigger from the profile module
  observe({
    req(profile_triggers)
    observeEvent(profile_triggers$go_login(), {
      current_user(NULL) # Set user to NULL to log out
      nav_select("main_nav", "login")
    })
  })
} 

shinyApp(ui = ui, server = server)