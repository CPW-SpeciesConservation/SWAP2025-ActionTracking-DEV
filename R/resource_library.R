library(shiny)
library(bslib)
library(dplyr)
library(memoise)
library(cachem)

# Local cache for the dropdowns
local_res_cache <- cachem::cache_mem(max_age = 3600)

get_res_tax_groups <- memoise(function(db) {
  dbGetQuery(db, "SELECT DISTINCT tg.groupname FROM proj.taxonomicgroups tg JOIN proj.species s ON tg.taxonomicgroupid = s.taxonomicgroupid JOIN track.specieshabitatactions sha ON s.speciesid = sha.speciesid ORDER BY tg.groupname")
}, cache = local_res_cache)

get_res_major_habitats <- memoise(function(db) {
  dbGetQuery(db, "SELECT DISTINCT mh.majorhabitatname FROM proj.majorhabitats mh JOIN proj.habitatsubtypes hs ON mh.majorhabitatid = hs.majorhabitatid JOIN track.specieshabitatactions sha ON hs.habitatsubtypeid = sha.habitatsubtypeid ORDER BY mh.majorhabitatname")
}, cache = local_res_cache)


resource_library_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Resource Library", class = "mb-4", style = "color: #07234C !important; font-weight: bold;"),
    fluidRow(class="mt-3",
             column(width = 4,
                    div(class = "card shadow-sm mb-3",
                        div(class = "card-header text-white", style = "background-color: #055A53;", "Search Library"),
                        div(class = "card-body",
                            radioButtons(ns("res_type"), "Search By:", choices = c("Species", "Habitat"), inline = TRUE),
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'Species'", ns("res_type")),
                              selectizeInput(ns("res_tax_group"), "Taxonomic Group", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                              selectizeInput(ns("res_species"), "Species", choices = c("Select a group first..." = ""),options = list(dropdownParent = "body"))
                            ),
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'Habitat'", ns("res_type")),
                              selectizeInput(ns("res_major_hab"), "Major Habitat", choices = c("Loading..." = ""),options = list(dropdownParent = "body")),
                              selectizeInput(ns("res_habitat"), "Habitat Subtype", choices = c("Select a major habitat first..." = ""),options = list(dropdownParent = "body"))
                            )
                        )
                    )
             ),
             column(width = 8,
                    div(class = "card shadow-sm",
                        div(class = "card-header text-white fw-bold", style = "background-color: #0D67B8;", "Available Resources"),
                        div(class = "card-body", style = "min-height: 300px;",
                            uiOutput(ns("res_library_content"))
                        )
                    )
             )
    )
  )
}

resource_library_server <- function(id, db, db_sync_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      db_sync_trigger() 
      tax_groups <- get_res_tax_groups(db)
      updateSelectInput(session, "res_tax_group", choices = c("Select group..." = "", tax_groups$groupname))
      habitats <- get_res_major_habitats(db)
      updateSelectInput(session, "res_major_hab", choices = c("Select habitat..." = "", habitats$majorhabitatname))
    })
    
    observeEvent(input$res_tax_group, {
      req(input$res_tax_group)
      query <- "
        SELECT s.speciesid, s.commonname 
        FROM proj.species s 
        JOIN proj.taxonomicgroups tg ON s.taxonomicgroupid = tg.taxonomicgroupid 
        WHERE tg.groupname = $1 
          AND (
            EXISTS (SELECT 1 FROM track.speciesresources sr WHERE sr.speciesid = s.speciesid)
            OR EXISTS (SELECT 1 FROM proj.SpeciesExistingActionsLinks sl WHERE sl.\"SpeciesID\" = s.speciesid)
          )
        ORDER BY s.commonname
      "
      res <- dbGetQuery(db, query, params = list(input$res_tax_group))
      updateSelectizeInput(session, "res_species", choices = c("Choose species..." = "", setNames(res$speciesid, res$commonname)))
    })
    
    observeEvent(input$res_major_hab, {
      req(input$res_major_hab)
      query <- "
        SELECT hs.habitatsubtypeid, hs.habitatsubtypename 
        FROM proj.habitatsubtypes hs 
        JOIN proj.majorhabitats mh ON hs.majorhabitatid = mh.majorhabitatid 
        WHERE mh.majorhabitatname = $1 
          AND (
            EXISTS (SELECT 1 FROM track.habitatresources hr WHERE hr.habitatsubtypeid = hs.habitatsubtypeid)
            OR EXISTS (SELECT 1 FROM proj.HabitatExistingActionsLinks hl WHERE hl.\"HabitatSubtypeID\" = hs.habitatsubtypeid)
          )
        ORDER BY hs.habitatsubtypename
      "
      res <- dbGetQuery(db, query, params = list(input$res_major_hab))
      updateSelectizeInput(session, "res_habitat", choices = c("Choose subtype..." = "", setNames(res$habitatsubtypeid, res$habitatsubtypename)))
    })
    
    output$res_library_content <- renderUI({
      db_sync_trigger()
      is_sp <- input$res_type == "Species"
      tid <- if(is_sp) input$res_species else input$res_habitat
      
      if (is.null(tid) || tid == "") return(p(class="text-muted text-center mt-5", "Select a target on the left to view available resources."))
      
      q_swap <- if(is_sp) {
        "SELECT \"Hyperlink Name\" AS name, \"Type\" AS type, \"Hyperlink URL\" AS url FROM proj.SpeciesExistingActionsLinks WHERE \"SpeciesID\" = $1 ORDER BY name"
      } else {
        "SELECT \"Hyperlink Name\" AS name, \"Type\" AS type, \"Hyperlink URL\" AS url FROM proj.HabitatExistingActionsLinks WHERE \"HabitatSubtypeID\" = $1 ORDER BY name"
      }
      swap_data <- dbGetQuery(db, q_swap, params = list(as.integer(tid)))
      
      q_add <- if(is_sp) {
        "SELECT resource_name AS name, resource_type AS type, resource_url AS url FROM track.speciesresources WHERE speciesid = $1 ORDER BY name"
      } else {
        "SELECT resource_name AS name, resource_type AS type, resource_url AS url FROM track.habitatresources WHERE habitatsubtypeid = $1 ORDER BY name"
      }
      add_data <- dbGetQuery(db, q_add, params = list(as.integer(tid)))
      
      if(nrow(swap_data) == 0 && nrow(add_data) == 0) return(p(class="text-muted", "No resources have been added for this selection yet."))
      
      build_resource_list <- function(data) {
        if(nrow(data) == 0) return(p(class="text-muted mb-3", em("No resources in this category.")))
        tags$ul(class = "list-group list-group-flush mb-4",
                lapply(1:nrow(data), function(i) {
                  icon_html <- if(grepl("Document|Plan|Outline", data$type[i], ignore.case=TRUE)) icon("file-pdf") else icon("link")
                  tags$li(class = "list-group-item px-0", style="background-color: transparent;",
                          icon_html, 
                          tags$a(href = data$url[i], target = "_blank", style = "margin-left: 10px; font-weight: bold; color: #0D67B8; text-decoration: none;", data$name[i]),
                          span(style = "color: grey; font-size: 0.85em; margin-left: 10px;", paste0("(", data$type[i], ")"))
                  )
                })
        )
      }
      
      tagList(
        div(h5("SWAP Resources", style = "color: #055A53; font-weight: bold; border-bottom: 2px solid #ECE8E4; padding-bottom: 5px;"), build_resource_list(swap_data)),
        div(h5("Additional Resources", style = "color: #AA5F40; font-weight: bold; border-bottom: 2px solid #ECE8E4; padding-bottom: 5px;"), build_resource_list(add_data))
      )
    })
    
  }) 
}