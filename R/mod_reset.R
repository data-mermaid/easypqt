#' reset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reset_ui <- function(id, show_ui = TRUE) {
  ns <- NS(id)

  reset <- warning_button(ns("reset"), "Restart")

  if (!show_ui) {
    reset <- shinyjs::hidden(reset)
  }

  shiny::div(
    reset
  )
}

#' reset Server Functions
#'
#' @noRd
mod_reset_server <- function(id, r, show_ui = TRUE, show_confirm = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!show_ui) {
      # Emulate a click on the UI
      shiny::observe({
        shinyjs::click("reset")
      })
    }

    # Only show the confirmation dialog if show_confirm = TRUE (e.g. not after ingestion success or failure)
    if (show_confirm) {
      shiny::observe({
        browser()
        show_modal(
          get_copy("reset", "title"),
          spaced(
            left_right(
              warning_button(ns("reset_confirm"), get_copy("reset", "confirm")),
              button(ns("reset_cancel"), get_copy("reset", "cancel"))
            )
          ),
          footer = NULL
        )
      }) %>%
        shiny::bindEvent(input$reset)

      shiny::observe({
        r$reset_confirm_counter <- r$reset_confirm_counter + 1
      }) %>%
        shiny::bindEvent(input$reset_confirm)
    } else {
      # Otherwise, just trigger the counter reset
      shiny::observe({
          r$reset_confirm_counter <- r$reset_confirm_counter + 1
      }) %>%
        shiny::bindEvent(input$reset)
    }
    # Otherwise, mimic it and trigger the reactive instead


    shiny::observe({
      shiny::req(r$reset_confirm_counter > 0)

      # To refrain from needing to do req(r$reset > 0), since bindEvent will not pick it up if it's NULL
      if (is.null(r$reset)) {
        r$reset <- 1
      } else {
        r$reset <- r$reset + 1
      }

      # Hide the modal
      if (show_confirm) {
        shiny::removeModal()
      }

      # General resets
      r$upload_contains_required_cols <- FALSE
      r$step_select_valid_project_done <- FALSE
      r$step_upload_valid_data_done <- FALSE
      r$step_map_auxiliary_fields_accordion_made_done <- FALSE
      r$step_map_auxiliary_fields_done <- FALSE
      r$step_map_auxiliary_fields_valid_done <- FALSE
      r$step_map_auxiliary_fields_accordion_fully_done <- FALSE
      r$step_map_coralnet_labels_accordion_made_done <- FALSE
      r$step_map_coralnet_labels_done <- FALSE
      r$step_map_coralnet_labels_fully_done <- FALSE
      r$preview_confirm_shown <- 0
      r$reset_confirm_counter <- 0
    }) %>%
      shiny::bindEvent(r$reset_confirm_counter)

    # Just close the modal on cancel
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
