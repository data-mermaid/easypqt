#' parse_annotations_check_valid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_check_valid_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' parse_annotations_check_valid Server Functions
#'
#' @noRd
mod_parse_annotations_check_valid_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
      shiny::req(r$valid_values)
      browser()
    })
  })
}

## To be copied in the UI
# mod_parse_annotations_check_valid_ui("parse_annotations_check_valid_1")

## To be copied in the server
# mod_parse_annotations_check_valid_server("parse_annotations_check_valid_1")
