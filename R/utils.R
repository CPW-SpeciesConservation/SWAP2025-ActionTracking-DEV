# R/utils.R — Shared helper functions used across modules

get_status_badge <- function(status) {
  if (is.null(status) || is.na(status)) return(span(class = "badge bg-secondary", "Unknown"))
  color <- switch(status,
                  "Completed"          = "bg-success",
                  "Achieved"           = "bg-success",
                  "On-track"           = "bg-success",
                  "Minor issues"       = "bg-warning text-dark",
                  "Partially achieved" = "bg-warning text-dark",
                  "Major issues"       = "bg-danger",
                  "Not achieved"       = "bg-danger",
                  "Abandoned"          = "bg-danger",
                  "bg-secondary"
  )
  span(class = paste("badge", color), status)
}