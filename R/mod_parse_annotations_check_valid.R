#' parse_annotations_check_valid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_check_valid_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' parse_annotations_check_valid Server Functions
#'
#' @noRd
mod_parse_annotations_check_valid_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Check that site/management are ones already entered in the project ----
    shiny::observe({
      shiny::req(r$valid_values)
      shiny::req(id %in% c("site", "management"))

      options_lookup <- r$auxiliary_columns_map[[id]][["column"]]

      valid_values <- r$valid_values[[options_lookup]][["value"]] # TODO, "value" in config?
      actual_values <- unique(r$annotations[[options_lookup]])

      # Check if values match
      # If all match, list values in data, and values in project?
      # If some match, list values that DO NOT match in data, values that do, and values in project
      # If none match, list values in data, and values in project
    })

    # Check that transect number is an integer ----
    shiny::observe({
      shiny::req(r$valid_values)
      shiny::req(id %in% c("transect_number"))

      # If all are integers, show transect numbers
      # If some are integers, show ones that ARE NOT and ones that are
      # If none are integers, show ones that ARE NOT
    })

    # Check that dates are all not empty ----
    shiny::observe({
      shiny::req(r$valid_values)
      shiny::req(id %in% c("date"))
    })
  })
}

## To be copied in the UI
# mod_parse_annotations_check_valid_ui("parse_annotations_check_valid_1")

## To be copied in the server
# mod_parse_annotations_check_valid_server("parse_annotations_check_valid_1")
