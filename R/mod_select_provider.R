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

  shiny::tagList(
    shiny::div(
      shiny::h2(get_copy("select_provider", "title")),
      spaced(get_copy("select_provider", "text")),
      shinyWidgets::radioGroupButtons(
        inputId = ns("provider"),
        choices = names(get_config("provider")),
        selected = character(0),
        individual = TRUE,
      )
    ),
    shiny::uiOutput(ns("provider_instructions"))
  )
}

#' select_provider Server Functions
#'
#' @noRd
mod_select_provider_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      # Update r$provider with selected provider ----
      r$provider_full <- input$provider

      # Update provider machine name (no caps etc)
      r$provider <- get_config("provider")[[r$provider_full]]

      # Disable provider selection
      shinyjs::disable("provider")
    }) %>%
      shiny::bindEvent(input$provider)

    output$provider_instructions <- shiny::renderUI({
      # Once provider is selected, show the introduction for it
      shiny::req(r$provider)

      shiny::div(class = "provider-instructions",
        shiny::hr(),
        get_copy("provider_introduction", r$provider),
        mod_upload_instructions_ui("instructions")
      )
    })

    # Instructions server ----

    # Reset and re-enable provider selection on reset ----
    shiny::observe({
      shinyjs::enable("provider")

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "provider",
        selected = character(0)
      )

      # Hide the provider intro
      shinyjs::hide(selector = ".provider-instructions")
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
