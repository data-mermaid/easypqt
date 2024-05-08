#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  authenticated <- shiny::reactiveVal(FALSE)

  # Set up reactive values ----
  r <- shiny::reactiveValues(
    required_annotations_columns = get_config("required_annotations_columns"),
    auxiliary_columns = get_config("auxiliary_columns"),
    auxiliary_columns_map = get_config("auxiliary_columns_map")
  )

  # Authenticate ----
  mod_authenticate_server("authenticate", r)

  # Get projects ----
  # This will also get the project template/options, and flag if they are not an admin of the selected project
  mod_select_project_server("select_project", r)

  # Upload CoralNet annotations ----
  # (only once confirmed that they are a project admin)
  mod_upload_annotations_server("upload_annotations", r)

  # Parse CoralNet annotations auxiliary fields ----
  mod_parse_annotations_aux_fields_server("parse_annotations_aux_fields", r)

  # Pull down project template with valid values of fields ----
  shiny::observe({
    shiny::req(r$aux_mapped)

    template_and_options <- mermaidr::mermaid_import_get_template_and_options(r$project, "benthicpqt")

    r$template <- template_and_options$Template

    r$valid_values <- template_and_options[unlist(r$auxiliary_columns_map %>% purrr::map("column"))] %>%
      purrr::compact() %>%
      purrr::map("choices")
  })

  # Check valid values of fields ----
  mod_parse_annotations_check_valid_server("site", r)
  mod_parse_annotations_check_valid_server("management", r)
  mod_parse_annotations_check_valid_server("transect_number", r)

  # Check CoralNet mapping ----
  # mod_check_coralnet_mermaid_mapping_server("check_mapping", r)

  # Check auxiliary fields ----
}
