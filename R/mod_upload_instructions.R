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
mod_upload_instructions_server <- function(id, show_ui = TRUE) {
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
      shiny::showModal(
        shiny::modalDialog(
          if (!show_ui) {
            shiny::tags$p(get_copy("upload_data", "missing_instructions"))
          },
          shiny::tags$p(get_copy("upload_data", "instructions")),
          shiny::tags$img(
            src = get_config("upload_data_img_path"),
            alt = get_copy("upload_data", "instructions_img_alt"),
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
