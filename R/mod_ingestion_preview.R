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
  shiny::tagList()
}

#' ingestion_preview Server Functions
#'
#' @noRd
mod_ingestion_preview_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preview and download reshaped data----
    output$table <- DT::renderDT({
      shiny::req(r$ingestion_data)
      DT::datatable(r$ingestion_data, rownames = FALSE, options = list(dom = "tp"))
    })

    shiny::observe({
      shiny::req(r$ingestion_data)
      # Only do this once, not every time the mapping is updated/confirmed
      shiny::req(r$preview_shown == 0)

      download <- shiny::downloadButton(ns("download_ingestion"), "Download reshaped data")

      r$accordion_preview_download <- bslib::accordion_panel(
        title = shiny::h2("Preview ingestion data"),
        value = "preview-download",
        indent(
          DT::DTOutput(ns("table")),
          download
        )
      )
      r$preview_shown <- r$preview_shown + 1
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
