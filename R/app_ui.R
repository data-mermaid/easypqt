#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      mod_authenticate_ui("authenticate"),
      mod_select_project_ui("select_project"),
      mod_upload_coralnet_data_ui("upload_coralnet_data"),
      mod_check_coralnet_mermaid_mapping_ui("check_mapping")
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
    )
  )
}
