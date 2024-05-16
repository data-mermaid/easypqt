#' ingestion_do UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ingestion_do_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ingestion_do Server Functions
#'
#' @noRd 
mod_ingestion_do_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ingestion_do_ui("ingestion_do_1")
    
## To be copied in the server
# mod_ingestion_do_server("ingestion_do_1")
