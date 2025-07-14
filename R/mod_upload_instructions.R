#' upload_instructions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_instructions_ui <- function(id, show_ui = TRUE) {
  ns <- NS(id)

  input <- shiny::actionLink(ns("help"), shiny::icon("info-circle"))

  if (!show_ui) {
    input <- shinyjs::hidden(input)
  }

  input
}

#' upload_instructions Server Functions
#'
#' @noRd
mod_upload_instructions_server <- function(id, r, show_ui = TRUE, invalid = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Trigger a click of the UI if it is not explicitly shown, but the upload instructions module is called
    if (!show_ui) {
      shiny::observe({
        shinyjs::click("help")
      })
    }

    shiny::observe({
      # If show_ui is FALSE, that means the instructions were called without the user explicitly asking, e.g. they uploaded the wrong data - so show the text that states that is the case
      cat("Upload instructions \n")
      shiny::showModal(
        shiny::modalDialog(
          if (!show_ui & !invalid) {
            shiny::tags$p(get_copy("upload_data", "missing_instructions", r$provider_machine))
          },
          if (invalid) {
            shiny::tags$p(get_copy("upload_data", "invalid_instructions", r$provider_machine))
          },
          shiny::tags$p(get_copy("upload_data", "instructions", r$provider_machine)),
          shiny::tags$img(
            src = get_config("upload_data_img_path")[[r$provider_machine]],
            alt = get_copy("upload_data", "instructions_img_alt", r$provider_machine),
            style = "width: 100%"
          ),
          footer = close_button,
          size = "l",
          easyClose = TRUE
        )
      )
    }) %>%
      shiny::bindEvent(input$help)
  })
}

## To be copied in the UI
# mod_upload_instructions_ui("upload_instructions_1")

## To be copied in the server
# mod_upload_instructions_server("upload_instructions_1")
