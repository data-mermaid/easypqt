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
    auxiliary_columns_map = get_config("auxiliary_columns_map"),
    auxiliary_columns_mapping = list(site = NULL, management = NULL, transect_number = NULL)
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

  # Check CoralNet mapping ----
  mod_check_coralnet_mermaid_mapping_server("check_mapping", r)

  # Check auxiliary fields ----
}
