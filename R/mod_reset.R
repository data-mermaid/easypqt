#' reset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reset_ui <- function(id) {
  ns <- NS(id)
  shiny::div(
    shiny::actionButton(ns("reset"), "Restart")
  )
}

#' reset Server Functions
#'
#' @noRd
mod_reset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      show_modal(
        get_copy("reset", "title"),
        shiny::actionButton(ns("reset_confirm"), get_copy("reset", "confirm")),
        shiny::actionButton(ns("reset_cancel"), get_copy("reset", "cancel")),
        footer = NULL
      )
    }) %>%
      shiny::bindEvent(input$reset)


    shiny::observe({
      session$reload()
    }) %>%
      shiny::bindEvent(input$reset_confirm)

    shiny::observe({
      shiny::removeModal()
    }) %>%
      shiny::bindEvent(input$reset_cancel)
  })
}

## To be copied in the UI
# mod_reset_ui("reset_1")

## To be copied in the server
# mod_reset_server("reset_1")
