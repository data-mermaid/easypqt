get_config <- function(key) {
  yaml::read_yaml(app_sys("config.yml"))[[key]]
}

get_copy <- function(key, secondary_key = NULL) {
  copy <- yaml::read_yaml(app_sys("copy.yml"))
  if (is.null(secondary_key)) {
    copy <- copy[[key]]
  } else {
    copy <- copy[[key]][[secondary_key]]
  }

  shiny::HTML(copy)
}

close_button <- shiny::modalButton("Close") # TODO copy

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

indent <- function(...) {
  shiny::div(class = "indent", ...)
}

spaced <- function(...) {
  shiny::div(class = "space", ...)
}

large <- function(...) {
  shiny::div(class = "large", ...)
}

left_right <- function(...) {
  shiny::div(class = "left-right", ...)
}

primary_button <- function(id, label, ...) {
  shinyWidgets::actionBttn(
    id,
    label,
    color = "primary",
    style = "simple",
    ...
  )
}

warning_button <- function(id, label, ...) {
  shinyWidgets::actionBttn(
    id,
    label,
    color = "danger",
    style = "simple",
    ...
  )
}

success_button <- function(id, label, ...) {
  shinyWidgets::actionBttn(
    id,
    label,
    color = "success",
    style = "simple",
    ...
  )
}

button <- function(id, label, ...) {
  shinyWidgets::actionBttn(
    id,
    label,
    style = "simple",
    ...
  )
}

disable_picker_input <- function(id) {
  shinyjs::runjs(glue::glue("let selector = $('#{id}'); selector.prop('disabled', true); selector.selectpicker('destroy'); selector.selectpicker();"))
}

enable_picker_input <- function(id) {
  shinyjs::runjs(glue::glue("let selector = $('#{id}'); selector.prop('disabled', false); selector.selectpicker('destroy'); selector.selectpicker();"))
}
