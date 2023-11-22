#' upload_coralnet_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_upload_coralnet_data_ui <- function(id) {
  ns <- NS(id)

  shiny::uiOutput(ns("upload"))
}

#' upload_coralnet_data Server Functions
#'
#' @noRd
mod_upload_coralnet_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show upload form once confirmed they are a project admin
    output$upload <- shiny::renderUI({
      shiny::req(r$is_project_admin)

      shiny::fileInput(ns("coralnet_data"), "Upload CoralNet data", accept = ".csv")
    })

    # I don't have data yet - so I'll just fake a data set that has some right/wrong
    shiny::observe({ # TODO - needs to depend on input$coralnet_data later
      shiny::req(r$is_project_admin)
      r$project

      r$coralnet_upload <- fake_data
    })
  })
}

fake_data <- dplyr::tribble(
  ~coralnet_label,
  "psa",
  "Anem",
  "cyp",
  "SC",
  "por",
  "test"
)
