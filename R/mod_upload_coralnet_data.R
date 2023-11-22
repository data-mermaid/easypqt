#' upload_coralnet_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_coralnet_data_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' upload_coralnet_data Server Functions
#'
#' @noRd
mod_upload_coralnet_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_upload_coralnet_data_ui("upload_coralnet_data_1")

## To be copied in the server
# mod_upload_coralnet_data_server("upload_coralnet_data_1")
