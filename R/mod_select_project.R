#' select_project UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_select_project_ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("select_project"))
}

#' select_project Server Functions
#'
#' @noRd
mod_select_project_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Once authenticated, get a list of projects to choose from ----
    shiny::observe({
      shiny::req(r$authenticated)

      r$projects <- mermaidr::mermaid_get_my_projects()
    })

    # Create project selection dropdown based on user's projects ----
    output$select_project <- shiny::renderUI({
      shiny::req(r$projects)

      projects <- setNames(r$projects$id, r$projects$name)

      shinyWidgets::pickerInput(
        ns("project"),
        "Select a project to ingest for",
        choices = projects,
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          liveSearch = TRUE,
          size = 10,
          maxOptions = 1,
          noneSelectedText = "Search project..."
        )
      )
    })

    # Update r$project with selected project ----
    shiny::observe(r$project <- input$project)

    # Get project template/options ----
    # At this point, will get an error if they are not an admin
    shiny::observeEvent(input$project, {
      template_and_options <- safely_get_template_and_options(input$project, "benthicpqt")

      r$is_project_admin <- check_project_admin(template_and_options)

      if (!r$is_project_admin) {
        show_not_project_admin_modal(r)
      } else {
        r$template_and_options <- template_and_options$result
      }
    })
  })
}

## To be copied in the UI
# mod_select_project_ui("select_project")

## To be copied in the server
# mod_select_project_server("select_project", r)

safely_get_template_and_options <- purrr::safely(mermaidr::mermaid_import_get_template_and_options)

check_project_admin <- function(response) {
  if (!is.null(response$error)) {
    if (response$error$parent$parent$parent$message == "Mermaid API request failed: (403) Forbidden") {
      FALSE
    } else {
      browser()
    }
  } else {
    TRUE
  }
}

show_not_project_admin_modal <- function(r) {
  project_name <- r$projects %>%
    dplyr::filter(id == r$project) %>%
    dplyr::pull(name)

  shiny::showModal(
    shiny::modalDialog(
      title = glue::glue("Unable to ingest into project {project_name}"),
      "You are not an admin in this project and will not be able to ingest until added as one"
    )
  )
}
