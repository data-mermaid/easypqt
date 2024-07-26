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
    warning_button(ns("reset"), "Restart")
  )
}

#' reset Server Functions
#'
#' @noRd
mod_reset_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
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
      # shinyjs::runjs("window.history.pushState({}, document.title, window.location.pathname);") # Remove code etc from URL so it can restart cleanly

      # Things to reset:
      # Auxiliary fields mapping
      # Parse
      # Reshape
      # Preview/confirm
      # Ingestion

      # DONE
      # Accordions
      # Project selection
      # Upload

      # shinyjs::refresh()
      # To refrain from needing to do req(r$reset > 0), since bindEvent will not pick it up if it's NULL
      if (is.null(r$reset)) {
        r$reset <- 1
      } else {
        r$reset <- r$reset + 1
      }
      # Hide the modal
      shiny::removeModal()

      # Some general resets:
      r$annotations_raw <- NULL
      r$ready_to_map_aux <- FALSE
      r$auxiliary_columns_map <- get_config("auxiliary_columns_map")
      r$all_aux_fields_valid <- FALSE
      r$aux_mapping_ui_created <- FALSE
      r$accordion_map_annotation_made <- FALSE
      r$map_annotations_accordion_made <- FALSE
      r$aux_mapped <- FALSE
      r$ingestion_data <- NULL
      r$annotations_mapped <- NULL
      r$preview_confirm_shown <- 0
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
