#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      # Authenticate ----
      mod_authenticate_ui("authenticate"),
      # Get projects ----
      mod_select_project_ui("select_project"),
      # Upload CoralNet annotations ----
      mod_upload_annotations_ui("upload_annotations"),
      # Parse CoralNet annotations auxiliary fields ----
      mod_parse_annotations_aux_fields_ui("parse_annotations_aux_fields"),
      # Check valid values of fields ----
      mod_parse_annotations_check_valid_ui("site"),
      mod_parse_annotations_check_valid_ui("management"),
      mod_parse_annotations_check_valid_ui("transect_number")
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  shiny::tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mermaidrcoralnet"
    ),
    shinyjs::useShinyjs()
  )
}
