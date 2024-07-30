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
  mod_reset_ui(ns("reset"), show_ui = FALSE)
}

#' ingestion_preview Server Functions
#'
#' @noRd
mod_ingestion_preview_and_confirm_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preview and download reshaped data ----

    output$table <- DT::renderDataTable(server = TRUE, {
      shiny::req(r$step_map_coralnet_labels_fully_done)
      DT::datatable(r$ingestion_data, rownames = FALSE, options = list(dom = "tp"), selection = "none")
    })

    shiny::observe({
      shiny::req(r$step_map_coralnet_labels_fully_done)
      # Only do this once, not every time the mapping is updated/confirmed
      shiny::req(r$preview_confirm_shown == 0)

      download <- shiny::downloadButton(ns("download_ingestion"), get_copy("preview", "download"))

      # Confirm proceeding to ingestion -----

      ## Correct: continue -----
      continue_button <- success_button(ns("correct_continue"), get_copy("ingestion", "continue_button")) %>%
        shiny::div(class = "space")

      ## Incorrect: start over -----
      incorrect_reset_button <- warning_button(ns("incorrect_reset"), get_copy("ingestion", "reset_button")) %>%
        shiny::div(class = "space")

      ## Incorrect: need help -----
      incorrect_help_button <- button(
        ns("incorrect_help"),
        get_copy("ingestion", "help_button"),
        onclick = glue::glue("window.open('{link}', '_blank')", link = get_copy("ingestion", "help_link"))
      ) %>%
        shiny::div(class = "space")

      r$accordion_preview_download_confirm <- bslib::accordion_panel(
        title = shiny::h2(get_copy("preview", "title")),
        value = "preview-download-confirm",
        shiny::tagList(
          spaced(get_copy("preview", "text")),
          indent(
            left_right(
              shiny::div(),
              download
            ),
            DT::DTOutput(ns("table")),
            shiny::div(get_copy("ingestion", "continue")),
            continue_button,
            shiny::div(get_copy("ingestion", "do_not_continue")),
            incorrect_reset_button,
            incorrect_help_button
          )
        )
      )
      r$preview_confirm_shown <- r$preview_confirm_shown + 1
    })

    output$download_ingestion <- shiny::downloadHandler(
      filename = function() {
        skeleton_to_text(get_copy("preview", "file"), list(project_name = r$project_name))
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
      mod_reset_server("reset", r, show_ui = FALSE)
    }) %>%
      shiny::bindEvent(input$incorrect_reset)

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
