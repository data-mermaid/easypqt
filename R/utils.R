get_config <- function(key) {
  yaml::read_yaml(app_sys("config.yml"))[[key]]
}

get_copy <- function(key) {
  copy <- get_config("copy")
  copy[[key]]
}

close_button <- shiny::modalButton("Close")

show_modal <- function(...) {
  shiny::showModal(
    shiny::modalDialog(
      ...,
      footer = close_button
    )
  )
}
