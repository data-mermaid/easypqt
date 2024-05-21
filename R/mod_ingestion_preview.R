#' ingestion_preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ingestion_preview_ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("preview_and_download"))
}

#' ingestion_preview Server Functions
#'
#' @noRd
mod_ingestion_preview_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preview and download reshaped data----
    output$preview_and_download <- shiny::renderUI({
      shiny::req(r$ingestion_data)
      table <- DT::datatable(r$ingestion_data, rownames = FALSE, options = list(dom = "tp")) %>%
        DT::renderDT()
      download <- shiny::downloadButton(ns("download_ingestion"), "Download reshaped data")

      shiny::tagList(
        shiny::h2("Preview ingestion data"),
        indent(
          table,
          download
        )
      )
    })

    output$download_ingestion <- shiny::downloadHandler(
      filename = function() {
        "test.csv" # TODO, name of project etc? with date?
      },
      content = function(file) {
        readr::write_csv(r$ingestion_data, file)
      }
    )
  })
}

## To be copied in the UI
# mod_ingestion_preview_ui("ingestion_preview_1")

## To be copied in the server
# mod_ingestion_preview_server("ingestion_preview_1")
