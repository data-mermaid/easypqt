#' parse_annotations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::div(),
    # Parse CoralNet annotations auxiliary fields ----
    mod_map_auxiliary_fields_ui(ns("map_aux")),
    # Show date/site/management, confirm and continue ----
    # TODO
    # Check valid values of fields ----
    mod_check_valid_values_ui(ns("valid_values")),
    mod_reset_ui(ns("reset"))
  )
}

#' parse_annotations Server Functions
#'
#' @noRd
mod_parse_annotations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      shiny::req(r$step_upload_valid_data_done)

      r$columns_map <- get_config("provider_columns_map")[[r$provider]]

      if (r$provider == "reefcloud") {
        # For ReefCloud: No need to map fields, just need to check that fields contain valid values
        r$step_fields_setup_done <- TRUE
        r$annotations <- r$annotations_raw # TODO -> why does this happen twice?
      } else if (r$provider == "coralnet") {
        # For CoralNet: Need to first map auxiliary fields to MERMAID fields
        mod_map_auxiliary_fields_server("map_aux", r)
      }
    })

    # Check valid values ----
    mod_check_valid_values_server("valid_values", r)
  })
}

## To be copied in the UI
# mod_parse_annotations_ui("parse_annotations_1")

## To be copied in the server
# mod_parse_annotations_server("parse_annotations_1")
