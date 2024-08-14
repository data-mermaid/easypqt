#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    waiter::useWaiter(),
    waiter::waiterShowOnLoad(
      html = shiny::h1(get_copy("loading")),
      color = colours[["currents_dark"]]
    ),
    bslib::page_fixed(
      theme = bslib::bs_theme(
        version = 5,
        primary = colours[["currents_dark"]]
      ) %>% bslib::bs_add_variables(
        "progress-bar-bg" = colours[["currents_dark"]],
        "link-hover-color" = colours[["currents_light"]],
        "link-color" = colours[["currents_dark"]],
        .where = "declarations"
      ),
      title = get_copy("title"),
      lang = "en",
      # Header
      shiny::div(
        # class = "sticky-header",
        shiny::h1(get_copy("title")),
        shiny::hr()
      ),
      large(
        spaced(
          get_copy("preamble"),
          mod_upload_instructions_ui("instructions"),
          shiny::HTML("</p>")
        )
      ),
      shiny::hr(),
      left_right(
        # Get projects ----
        mod_select_project_ui("select_project"),
        mod_reset_ui("reset")
      ),
      # Upload CoralNet annotations ----
      mod_upload_data_ui("upload_data"),
      # Parse CoralNet annotations ----
      bslib::accordion(
        id = "accordion",
        multiple = TRUE,
        mod_map_auxiliary_fields_ui("map_auxliary_fields"),
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
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = get_copy("title")
    ),
    shinyjs::useShinyjs()
  )
}
