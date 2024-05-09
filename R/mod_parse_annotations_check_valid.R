#' parse_annotations_check_valid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_check_valid_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' parse_annotations_check_valid Server Functions
#'
#' @noRd
mod_parse_annotations_check_valid_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check that site/management are ones already entered in the project ----
    shiny::observe({
      shiny::req(r$aux_mapped)
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
      shiny::req(r$aux_mapped)
      shiny::req(id %in% c("transect_number"))

      values <- r$annotations[r$auxiliary_columns_map[[id]][["column"]]]
      names(values) <- "value"

      values_with_numeric <- values %>%
        dplyr::mutate(numeric_value = as.integer(value) %>% suppressWarnings())

      invalid_values <- values_with_numeric %>%
        dplyr::filter(is.na(numeric_value))

      if (nrow(invalid_values) > 0) {
        # Show the invalid ones only

        transect_number_invalid_skeleton <- get_copy("transect_number_not_integer")
        invalid_values <- make_formatted_list(invalid_values[["value"]])

        show_modal(
          glue::glue(transect_number_invalid_skeleton, .envir = list(invalid_values = invalid_values))
        )
      } else {
        show_modal("transect numbers good")
        # TODO
      }
    })
  })
}

## To be copied in the UI
# mod_parse_annotations_check_valid_ui("parse_annotations_check_valid_1")

## To be copied in the server
# mod_parse_annotations_check_valid_server("parse_annotations_check_valid_1")
