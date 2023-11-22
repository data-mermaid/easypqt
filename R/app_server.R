#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  authenticated <- shiny::reactiveVal(FALSE)

  # Set up reactive values ----
  r <- shiny::reactiveValues(
    authenticated = FALSE,
    template_and_options = NULL
  )

  # Authenticate ----
  # Authenticate without caching token - available for usage in session only
  shiny::observe({
    mermaidr::mermaid_auth(cache = TRUE) # TODO - change to FALSE, just TRUE right now because authenticating over and over is annoying

    # Flag that authentication is done
    r$authenticated <- TRUE
  }) %>%
    shiny::bindEvent(input$auth)

  # Get projects ----
  # Once authenticated, get a list of projects to choose from
  # TODO - need to be an admin on selected project

  projects <- shiny::reactive({
    shiny::req(r$authenticated)

    mermaidr::mermaid_get_my_projects()
  })

  output$select_project <- shiny::renderUI({
    projects <- setNames(projects()$id, projects()$name)

    shinyWidgets::pickerInput(
      "project",
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

  # Get project template/options ----
  # At this point, will get an error if they are not an admin
  shiny::observeEvent(input$project, {
    template_and_options <- safely_get_template_and_options(input$project, "benthicpqt")
    is_project_admin <- check_project_admin(template_and_options)

    if (!is_project_admin) {
      project_name <- projects() %>%
        dplyr::filter(id == input$project) %>%
        dplyr::pull(name)

      shiny::showModal(
        shiny::modalDialog(
          title = glue::glue("Unable to ingest into project {project_name}"),
          "You are not an admin in this project and will not be able to ingest until added as one"
        )
      )
    }
  })
}

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
