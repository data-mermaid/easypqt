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

      instructions <- shiny::tagList(
        get_copy("provider_introduction", r$provider, "first"),
        mod_upload_instructions_ui("instructions"),
        get_copy("provider_introduction", r$provider, "second")
      )

      r$provider_instructions_done <- TRUE

      shiny::div(
        class = "provider-instructions",
        id = "provider-instructions",
        shiny::hr(),
        left_right(
          shiny::div(),
          mod_reset_ui("reset")
        ),
        shiny::br(),
        instructions
      )
    })

    # Scroll to provider instructions section
    shiny::observe({
      shiny::req(r$provider_instructions_done)
      scroll_to_section("provider-instructions")
    }) %>%
      shiny::bindEvent(r$provider_instructions_done)

    # Instructions server ----

    # Reset and re-enable provider selection on reset ----
    shiny::observe({
      shinyjs::enable("provider")

      shinyWidgets::updateRadioGroupButtons(
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
