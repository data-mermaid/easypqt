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
    dev = TRUE,
    # dev_scenario = "empties"
    # dev_scenario = "wrong_values"
    dev_scenario = "good_data"
    # dev_scenario = "some_good_some_wrong"
    # dev_scenario = "transect_decimal"
  )

  # Authenticate ----
  mod_authenticate_server("authenticate", r)

  # Get projects ----
  # This will also get the project template/options, and flag if they are not an admin of the selected project
  mod_select_project_server("select_project", r)

  # Upload CoralNet annotations ----
  # (only once confirmed that they are a project admin)
  mod_upload_annotations_server("upload_annotations", r)

  # Parse annotations -----
  # Map and check auxiliary fields
  mod_parse_annotations_server("parse_annotations", r)
}
