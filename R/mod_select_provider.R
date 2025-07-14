#' select_provider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_select_provider_ui <- function(id) {
  ns <- NS(id)

  shiny::div(
    shiny::h2(get_copy("select_provider", "title")),
    spaced(get_copy("select_provider", "text")),
    shinyWidgets::radioGroupButtons(
      inputId = ns("provider"),
      choices = names(get_config("provider")),
      selected = character(0),
      individual = TRUE,
    )
  )
}

#' select_provider Server Functions
#'
#' @noRd
mod_select_provider_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      browser()
      # Update r$provider with selected provider ----
      r$provider <- input$provider

      # Update provider machine name (no caps etc)
      r$provider_machine <- get_config("provider")[[r$provider]]

      # Disable provider selection
      disable_picker_input(ns("provider"))
    }) %>%
      shiny::bindEvent(input$provider)

    # Disable provider selection
    shiny::observe({
      shiny::req(r$ready_to_map_aux)
      disable_picker_input(ns("project"))
    }) %>%
      shiny::bindEvent(r$ready_to_map_aux)

    # Reset and re-enable provider selection on reset ----
    shiny::observe({
      enable_picker_input(ns("provider"))

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project",
        selected = character(0)
      )
    }) %>%
      shiny::bindEvent(r$reset)
  })
}

# Utils ----

show_not_project_admin_modal <- function(r) {
  project_role <- r$project_role


  cat("Not admin \n")
  show_modal(skeleton_to_text(get_copy("select_provider", "not_admin"), list(project_name = r$project_name, project_id = r$project, role = project_role)))
}
