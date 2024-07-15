#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    waiter::useWaiter(),
    waiter::waiterShowOnLoad(html = shiny::h1("Loading EasyPQT..."), color = "#174B82"),
    bslib::page_fixed(
      theme = bslib::bs_theme(version = 5, primary = "#174B82"),
      title = "Easy PQT",
      lang = "en",
      # Header
      shiny::div(
        # class = "sticky-header",
        shiny::h1("Easy PQT"),
        shiny::hr()
      ),
      # shiny::div(class="sticky-header-placeholder"),
      left_right(
        # Loading projects ----
        # shinyjs::hidden(
        # shiny::div(
        #   id = "loading-projects",
        #   style = "margin-top: 1rem",
        #   get_copy("authenticate", "loading")
        # ),
        # Get projects ----
        mod_select_project_ui("select_project"),
        # ),
        # Authenticate ----
        # mod_authenticate_ui("authenticate"),
        # Reset
        mod_reset_ui("reset"),
      ),
      # Upload CoralNet annotations ----
      mod_upload_annotations_ui("upload_annotations"),
      # Parse CoralNet annotations ----
      bslib::accordion(
        id = "accordion",
        multiple = TRUE,
        mod_parse_annotations_ui("parse_annotations"),
        # Reshape annotations ----
        mod_reshape_annotations_ui("reshape_annotations"),
        # Preview/confirm ingestion ----
        mod_ingestion_preview_and_confirm_ui("preview_and_confirm")
      ),
      # Do ingestion ----
      mod_ingestion_do_ui("ingest")

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
