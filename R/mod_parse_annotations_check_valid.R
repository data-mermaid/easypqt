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
  shiny::fluidRow(
    shiny::uiOutput(ns("check_results"))
  )
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
      shiny::req(r$no_empty_fields)
      shiny::req(id %in% c("site", "management"))

      options_lookup <- r$auxiliary_columns_map[[id]][["column"]]

      valid_values <- r$template_choices[[options_lookup]][["value"]] # TODO, "value" in config?
      actual_values <- unique(r$annotations[[options_lookup]])

      invalid_values <- setdiff(actual_values, valid_values)

      if (length(invalid_values) > 0) {
        invalid_values_skeleton <- get_copy("invalid_values")
        invalid_values <- make_formatted_list(invalid_values)
        valid_values <- make_formatted_list(valid_values)

        invalid_values_envir <- list(
          label = r$auxiliary_columns_map[[id]][["label"]],
          invalid_values = invalid_values,
          valid_values = valid_values
        )

        # show_modal(
        output$check_results <- skeleton_to_text(invalid_values_skeleton, invalid_values_envir) %>%
          shiny::renderUI()
        # )
      } else {
        show_modal(glue::glue("{id} values good"))
        # TODO
      }
    })

    # Check that transect number is an integer ----
    shiny::observe({
      shiny::req(r$aux_mapped)
      shiny::req(r$no_empty_fields)
      shiny::req(id %in% c("transect_number"))

      values <- r$annotations[r$auxiliary_columns_map[[id]][["column"]]]
      names(values) <- "value"

      values_with_numeric <- values %>%
        dplyr::mutate(numeric_value = as.integer(value) %>% suppressWarnings())

      invalid_values <- values_with_numeric %>%
        dplyr::filter(is.na(numeric_value) |
          as.character(numeric_value) != value) # Handles decimals

      if (nrow(invalid_values) > 0) {
        # Show the invalid ones only

        transect_number_invalid_skeleton <- get_copy("transect_number_not_integer")
        invalid_values <- make_formatted_list(invalid_values[["value"]])

        # show_modal(
        output$check_results <- skeleton_to_text(transect_number_invalid_skeleton, list(invalid_values = invalid_values)) %>%
          shiny::renderUI()
        # )
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
