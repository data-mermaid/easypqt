#' confirm_continue_ingestion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ingestion_confirm_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList()
}

#' confirm_continue_ingestion Server Functions
#'
#' @noRd
mod_ingestion_confirm_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI
    shiny::observe({
      # Only do this once, not every time the mapping is updated/confirmed
      shiny::req(r$preview_shown == 0)
      shiny::req(r$ingestion_data)

      # Confirm continuing with ingestion ----
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

      r$accordion_confirm <- bslib::accordion_panel(
        title = shiny::h2(get_copy("ingestion", "title")),
        value = "confirm",
        indent(
          shiny::div(get_copy("ingestion", "continue")),
          continue_button,
          shiny::div(get_copy("ingestion", "do_not_continue")),
          incorrect_reset_button,
          incorrect_help_button
        )
      )
      r$preview_shown <- r$preview_shown + 1
    })

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
        get_copy("reset"),
        shiny::actionButton(ns("reset_confirm"), get_copy("reset_confirm")),
        shiny::actionButton(ns("reset_cancel"), get_copy("reset_cancel")),
        footer = NULL
      )
    }) %>%
      shiny::bindEvent(input$incorrect_reset)

    shiny::observe({
      r$reset <- TRUE
      browser()
      # Each modal should listen to reset, and clear or return to its defaults
      # Or just set certain reactives to NULL so the renderUIs will reset everything?
      # TODO
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
# mod_confirm_continue_ingestion_ui("confirm_continue_ingestion_1")

## To be copied in the server
# mod_confirm_continue_ingestion_server("confirm_continue_ingestion_1")
