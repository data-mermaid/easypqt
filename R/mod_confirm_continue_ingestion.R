#' confirm_continue_ingestion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_confirm_continue_ingestion_ui <- function(id) {
  ns <- NS(id)
  # Confirm continuing with ingestion ----
  ## Correct: continue -----
  continue_button <- shiny::actionButton(ns("correct_continue"), "Continue with ingestion")

  ## Incorrect: edit options -----
  incorrect_edit_button <- shiny::actionButton(ns("incorrect_edit"), "Edit mapping")

  ## Incorrect: start over -----
  incorrect_reset_button <- shiny::actionButton(ns("incorrect_reset"), "Restart process")

  ## Incorrect: need help -----
  incorrect_help_button <- shiny::actionButton(
    ns("incorrect_help"),
    "Get help",
    onclick = "window.open('https://datamermaid.org/contact-us', '_blank')"
  )

  shiny::tagList(
    shiny::div("If the data looks correct, click the button below to continue with ingestion"),
    continue_button,
    shiny::div("If the data does not look correct, select from the following options to edit the existing mapping, restart the whole process, or get help with ingestion"),
    incorrect_edit_button,
    incorrect_reset_button,
    incorrect_help_button,
  )
}

#' confirm_continue_ingestion Server Functions
#'
#' @noRd
mod_confirm_continue_ingestion_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Correct: continue -----
    # Close modal, flag to do ingestion
    shiny::observe({
      shiny::removeModal()

      r$do_ingestion <- TRUE
    }) %>%
      shiny::bindEvent(input$correct_continue)

    ## Incorrect: edit options -----
    # Close modal, open a modal that says how to edit?
    shiny::observe({
      browser()
    }) %>%
      shiny::bindEvent(input$incorrect_edit)

    ## Incorrect: start over -----
    # Confirm to reset, clear everything
    shiny::observe({
      browser()
    }) %>%
      shiny::bindEvent(input$incorrect_reset)

    ## Incorrect: need help -----
    # Link to contact page, handled in UI
  })
}

## To be copied in the UI
# mod_confirm_continue_ingestion_ui("confirm_continue_ingestion_1")

## To be copied in the server
# mod_confirm_continue_ingestion_server("confirm_continue_ingestion_1")
