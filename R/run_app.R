#' Run the Shiny Application
#'
#' @export
#' @importFrom golem with_golem_options
run_app <- function(...) {
  options(shiny.port = 8080)

    with_golem_options(
    app = shinyAppAuth0(
      ui = app_ui(),
      server = app_server,
      config_file = system.file("app/_auth0.yml", package = "easypqt")
    ),
    golem_opts = list(...)
  )
}
