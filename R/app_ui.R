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
        logo_header(
          shiny::h1(get_copy("title")),
          shiny::tags$img(
            class = "logo",
            src = get_config("logo_img_path"),
            alt = get_copy("logo_alt")
          )
        ),
        shiny::hr()
      ),
      left_right(
        shiny::div(),
        mod_reset_ui("reset")
      ),
      large(
        spaced(
          get_copy("introduction"),
          shiny::hr(),
          mod_select_provider_ui("provider")
        )
      ),
      shiny::hr(),
      # Get projects ----
      mod_select_project_ui("select_project"),
      # Upload annotations ----
      mod_upload_data_ui("upload_data"),
      bslib::accordion(
        id = "accordion",
        multiple = TRUE,
        # Parse annotations ----
        mod_parse_annotations_ui("parse_annotations"),
        # Map labels to MERMAID ----
        mod_map_provider_labels_to_mermaid_ui("map_labels"),
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
    shiny::includeHTML(app_sys("app/www/ga.html")),
    favicon(ext = "svg"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = get_copy("title")
    ),
    shinyjs::useShinyjs(),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css?version=4")
  )
}
