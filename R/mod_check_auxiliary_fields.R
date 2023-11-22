#' check_auxiliary_fields UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_check_auxiliary_fields_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' check_auxiliary_fields Server Functions
#'
#' @noRd
mod_check_auxiliary_fields_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
