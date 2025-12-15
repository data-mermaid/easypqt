#' select_human_or_machine_annotated UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_select_human_or_machine_annotated_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("human_or_machine"))
  )
}

#' upload_data Server Functions
#'
#' @noRd
mod_select_human_or_machine_annotated_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # For reefcloud, show this selection once confirmed they are a project admin (they have selected a valid project),
    # and additionally on any reset
    # For coralnet, just automatically set step_select_human_or_machine_annotated <- TRUE here
    shiny::observe({
      shiny::req(r$step_select_valid_project_done)

      output$human_or_machine <- renderUI({
        shiny::req(r$provider)
        if (r$step_select_valid_project_done & r$provider == "reefcloud") {
          shiny::div(
            id = "select-human-machine",
            shiny::h2(get_copy("select_human_or_machine", "title")),
            spaced(get_copy("select_human_or_machine", "text")),
            shinyWidgets::radioGroupButtons(
              inputId = ns("human_or_machine"),
              choiceValues = names(get_config("human_or_machine")),
              choiceNames = unname(get_config("human_or_machine")),
              selected = character(0),
              individual = TRUE,
            ),
            shiny::hr()
          )
        } else {
          if (r$step_select_valid_project_done & r$provider == "coralnet") {
            r$step_select_human_or_machine_annotated <- TRUE
          }
          shiny::tagList()
        }
      })

      r$human_or_machine_ui_done <- TRUE
    }) %>%
      shiny::bindEvent(r$step_select_valid_project_done, r$reset)

    # Scroll to selection
    shiny::observe({
      shiny::req(r$human_or_machine_ui_done)
      scroll_to_section("select-human-machine")
    }) %>%
      shiny::bindEvent(r$human_or_machine_ui_done)

    # Set step_select_human_or_machine_annotated = TRUE once selected, if ReefCloud
    shiny::observe({
      shiny::req(input$human_or_machine)
      r$human_annotated_only <- ifelse(input$human_or_machine == "yes", TRUE, FALSE)
      r$step_select_human_or_machine_annotated <- TRUE

      # Disable selection
      shinyjs::disable("human_or_machine")
    }) %>%
      shiny::bindEvent(input$human_or_machine)

    # Reset and re-enable selection on reset ----
    shiny::observe({
      shinyjs::enable("human_or_machine")

      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "human_or_machine",
        selected = character(0)
      )
    }) %>%
      shiny::bindEvent(r$reset)
  })
}
