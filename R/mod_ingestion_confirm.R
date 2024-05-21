#' confirm_continue_ingestion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ingestion_confirm_ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("confirm_ui"))
}

#' confirm_continue_ingestion Server Functions
#'
#' @noRd
mod_ingestion_confirm_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI
    output$confirm_ui <- shiny::renderUI({
      shiny::req(r$ingestion_data)

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
        shiny::h2("Continue to ingestion"),
        indent(
          shiny::div("If the data looks correct, click the button below to continue with ingestion"),
          continue_button,
          shiny::div("If the data does not look correct, select from the following options to edit the existing mapping, restart the whole process, or get help with ingestion"),
          incorrect_edit_button,
          incorrect_reset_button,
          incorrect_help_button
        )
      )
    })

    ## Correct: continue -----
    # Flag to do ingestion
    shiny::observe({
      r$do_ingestion <- TRUE
    }) %>%
      shiny::bindEvent(input$correct_continue)

    ## Incorrect: edit options -----
    # Open a modal that says what to edit
    shiny::observe({
      show_modal("Return to the 'Map CoralNet annotation fields' and/or 'Map CoralNet Labels to MERMAID Attributes' sections to edit the data")
    }) %>%
      shiny::bindEvent(input$incorrect_edit)

    ## Incorrect: start over -----
    # Confirm to reset, clear everything
    shiny::observe({
      show_modal(
        "Are you sure you want to reset the project, data, and all mappings?",
        shiny::actionButton(ns("reset_confirm"), "Reset all"),
        shiny::actionButton(ns("reset_cancel"), "Do not reset ingestion"),
        footer = NULL
      )
    }) %>%
      shiny::bindEvent(input$incorrect_reset)


    shiny::observe({
      r$reset <- TRUE
      browser()
      # Each modal should listen to reset, and clear or return to its defaults
      # Or just set certain reactives to NULL so the renderUIs will reset everything?
      # TODO
    }) %>%
      shiny::bindEvent(input$reset_confirm)


    shiny::observe({
      browser()
      # TODO
    }) %>%
      shiny::bindEvent(input$reset_cancel)

    ## Incorrect: need help -----
    # Link to contact page, handled in UI
  })
}

## To be copied in the UI
# mod_confirm_continue_ingestion_ui("confirm_continue_ingestion_1")

## To be copied in the server
# mod_confirm_continue_ingestion_server("confirm_continue_ingestion_1")
