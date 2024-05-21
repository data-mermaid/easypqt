#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_fixed(
      theme = bslib::bs_theme(version = 5, primary = "#174B82"),
      title = "EasyPQT",
      lang = "en",
      shiny::h1("EasyPQT"),
      shiny::hr(),
      # Authenticate ----
      mod_authenticate_ui("authenticate"),
      # Get projects ----
      mod_select_project_ui("select_project"),
      # Upload CoralNet annotations ----
      mod_upload_annotations_ui("upload_annotations"),
      # Parse CoralNet annotations ----
      bslib::accordion(
        id = "accordion",
        multiple = FALSE,
        mod_parse_annotations_ui("parse_annotations"),
        # Reshape annotations ----
        # mod_reshape_annotations_ui("reshape_annotations"),
        # Preview ingestion ----
        # mod_ingestion_preview_ui("preview"),
        # Confirm ingestion ----
        # mod_ingestion_confirm_ui("confirm")
      )
      # Do ingestion ----

      # Ingestion results -----
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
      app_title = "Easy PQT"
    ),
    shinyjs::useShinyjs()
  )
}
