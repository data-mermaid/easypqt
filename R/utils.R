get_config <- function(key) {
  yaml::read_yaml(app_sys("config.yml"))[[key]]
}

get_copy <- function(key) {
  copy <- get_config("copy")
  copy[[key]]
}

close_button <- shiny::modalButton("Close")

confirm_button <- function(id) {
  shiny::actionButton(id, "Confirm")
}

modal <- function(..., title = NULL, footer, size = "m", disable_footer = FALSE) {
  if (disable_footer) {
    footer <- shinyjs::disabled(footer)
  }

  shiny::showModal(
    shiny::modalDialog(
      ...,
      title = title,
      footer = footer,
      size = size
    )
  )
}

show_modal <- function(..., footer = close_button) {
  modal(..., footer = footer)
}

confirm_modal <- function(..., footer_id, title = NULL, size = "l") {
  modal(..., title = title, footer = confirm_button(footer_id), size = size, disable_footer = TRUE)
}

make_formatted_list <- function(x) {
  shiny::tags$ul(
    purrr::map(x, shiny::tags$li)
  )
}

skeleton_to_text <- function(skeleton, envir) {
  glue::glue(skeleton, .envir = envir) %>%
    shiny::HTML()
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

open_css <- function() {
  usethis::edit_file("inst/app/www/styles.css")
}

null_if_dev <- function(dev, non_null) {
  if (dev) {
    non_null
  } else {
    NULL
  }
}

indent_input <- function(input) {
  shiny::div(class = "indent-input", input)
}

primary_button <- function(id, label) {
  shinyWidgets::actionBttn(id,
    label,
    color = "primary",
    style = "simple"
  )
}
