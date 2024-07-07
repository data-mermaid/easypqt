#' authenticate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_authenticate_ui <- function(id) {
  ns <- NS(id)
    primary_button(
      ns("auth"),
      get_copy("authenticate", "title")
    )
}

#' authenticate Server Functions
#'
#' @noRd
mod_authenticate_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Authenticate without caching token - available for usage in session only
    shiny::observe({
      mermaidr::mermaid_auth(cache = FALSE)

      # Flag that authentication is done
      r$authenticated <- TRUE

      # Disable authentication button
      shinyjs::disable("auth")

      # Show project loading text
      shinyjs::show("loading-projects", asis = TRUE)
    }) %>%
      shiny::bindEvent(input$auth)
  })
}
