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
    # Parse CoralNet annotations auxiliary fields ----
    mod_parse_annotations_aux_fields_ui(ns("parse_annotations_aux_fields")),
    # Show date/site/management, confirm and continue ----
    # TODO
    # Check valid values of fields ----
    mod_parse_annotations_check_valid_ui(ns("site")),
    mod_parse_annotations_check_valid_ui(ns("management")),
    mod_parse_annotations_check_valid_ui(ns("transect_number"))
  )
}

#' parse_annotations Server Functions
#'
#' @noRd
mod_parse_annotations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Map auxiliary fields ----
    mod_parse_annotations_aux_fields_server("parse_annotations_aux_fields", r)

    # Show date/site/management, confirm and continue ----
    # TODO

    # # Pull down project template with valid values of fields ----
    # shiny::observe({
    #   # shiny::req(r$aux_mapped)
    #   shiny::req(r$project)
    #
    #   template_and_options <- mermaidr::mermaid_import_get_template_and_options(r$project, "benthicpqt") # TODO, "benthicpqt" in config?
    #
    #   r$template <- template_and_options$Template
    #
    #   r$valid_values <- template_and_options[unlist(r$auxiliary_columns_map %>% purrr::map("column"))] %>%
    #     purrr::compact() %>%
    #     purrr::map("choices")
    # })

    # Check that fields are not empty ----
    mod_parse_annotations_check_nonempty_server("check_nonempty", r)

    # Check valid values of fields ----
    mod_parse_annotations_check_valid_server("site", r)
    mod_parse_annotations_check_valid_server("management", r)
    mod_parse_annotations_check_valid_server("transect_number", r)

    # Check CoralNet mapping ----
    # mod_check_coralnet_mermaid_mapping_server("check_mapping", r)

    # Check auxiliary fields ----
  })
}

## To be copied in the UI
# mod_parse_annotations_ui("parse_annotations_1")

## To be copied in the server
# mod_parse_annotations_server("parse_annotations_1")
