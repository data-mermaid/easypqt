#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  authenticated <- shiny::reactiveVal(FALSE)

  # Set up reactive values ----
  r <- shiny::reactiveValues()

  # Authenticate ----
  mod_authenticate_server("authenticate", r)

  # Get projects ----
  # This will also get the project template/options, and flag if they are not an admin of the selected project
  mod_select_project_server("select_project", r)

  # Upload CoralNet data ----
  # (only once confirmed that they are a project admin)
  mod_upload_annotations_server("upload_annotations", r)

  # Check CoralNet mapping ----
  mod_check_coralnet_mermaid_mapping_server("check_mapping", r)

  # Check auxiliary fields ----
}
