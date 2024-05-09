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

make_formatted_list <- function(x) {
  shiny::tags$ul(
    purrr::map(x, shiny::tags$li)
  )
}

open_file <- function(file) {
  usethis::use_r(file)
}

open_server <- function() {
  open_file("app_server")
}

open_ui <- function() {
  open_file("app_ui")
}

open_utils <- function() {
  open_file("utils")
}

open_config <- function() {
  usethis::edit_file("inst/config.yml")
}
