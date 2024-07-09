shinyAppAuth0 <- function(ui, server, config_file = NULL, ...) {
  disable <- getOption("auth0_disable")
  if (!is.null(disable) && disable) {
    shiny::shinyApp(ui, server, ...)
  } else {
    if (is.null(config_file)) {
      config_file <- auth0::auth0_find_config_file()
    }
    info <- auth0::auth0_info(config_file)
    shiny::shinyApp(auth0::auth0_ui(ui, info), auth0_server(server, info), ...)
  }
}
