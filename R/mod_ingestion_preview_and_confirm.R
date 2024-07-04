#' ingestion_preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ingestion_preview_and_confirm_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList()
}

#' ingestion_preview Server Functions
#'
#' @noRd
mod_ingestion_preview_and_confirm_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preview and download reshaped data ----

    output$table <- DT::renderDataTable(server = TRUE, {
      shiny::req(r$ingestion_data)
      DT::datatable(r$ingestion_data, rownames = FALSE, options = list(dom = "tp"), selectionm = "none")
    })

    shiny::observe({
      shiny::req(r$ingestion_data)
      # Only do this once, not every time the mapping is updated/confirmed
      shiny::req(r$preview_confirm_shown == 0)

      download <- shiny::downloadButton(ns("download_ingestion"), "Download reshaped data")

      # Confirm proceeding to ingestion -----

      ## Correct: continue -----
      continue_button <- shiny::actionButton(ns("correct_continue"), get_copy("ingestion", "continue_button"))

      ## Incorrect: start over -----
      incorrect_reset_button <- shiny::actionButton(ns("incorrect_reset"), get_copy("ingestion", "reset_button"))

      ## Incorrect: need help -----
      incorrect_help_button <- shiny::actionButton(
        ns("incorrect_help"),
        get_copy("ingestion", "help_button"),
        onclick = glue::glue("window.open('{link}', '_blank')", link = get_copy("https://datamermaid.org/contact-us"))
      )

      r$accordion_preview_download_confirm <- bslib::accordion_panel(
        title = shiny::h2("Preview data and confirm ingestion"), # TODO config
        value = "preview-download-confirm",
        indent(
          DT::DTOutput(ns("table")),
          download,
          shiny::div(get_copy("ingestion", "continue")),
          continue_button,
          shiny::div(get_copy("ingestion", "do_not_continue")),
          incorrect_reset_button,
          incorrect_help_button
        )
      )
      r$preview_confirm_shown <- r$preview_confirm_shown + 1
    })

    output$download_ingestion <- shiny::downloadHandler(
      filename = function() {
        "test.csv" # TODO, name of project etc? with date?
      },
      content = function(file) {
        readr::write_csv(r$ingestion_data, file)
      }
    )

    ## Correct: continue -----
    # Flag to do ingestion
    shiny::observe({
      r$do_ingestion <- TRUE
    }) %>%
      shiny::bindEvent(input$correct_continue)

    ## Incorrect: start over -----
    # Confirm to reset, clear everything
    shiny::observe({
      show_modal(
        get_copy("reset", "title"),
        shiny::actionButton(ns("reset_confirm"), get_copy("reset", "confirm")),
        shiny::actionButton(ns("reset_cancel"), get_copy("reset", "cancel")),
        footer = NULL
      )
    }) %>%
      shiny::bindEvent(input$incorrect_reset)

    shiny::observe({
      session$reload()
    }) %>%
      shiny::bindEvent(input$reset_confirm)

    shiny::observe({
      shiny::removeModal()
    }) %>%
      shiny::bindEvent(input$reset_cancel)

    ## Incorrect: need help -----
    # Link to contact page, handled in UI
  })
}

## To be copied in the UI
# mod_ingestion_preview_ui("ingestion_preview_1")

## To be copied in the server
# mod_ingestion_preview_server("ingestion_preview_1")
