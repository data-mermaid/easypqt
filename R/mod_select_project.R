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

    # Show project selection once they have selected a provider ----
    output$select_project <- shiny::renderUI({
      shiny::req(r$provider)

      # Use users' projects to make dropdown
      projects <- setNames(r$projects$id, r$projects$name)

      # If they have no projects, show a modal with this information - they need a project in order to use easyPQT!
      if (length(projects) == 0) {
        show_modal(get_copy("select_project", "no_projects"))
      }

      shiny::div(
        class = "project-selection",
        shiny::h2(get_copy("select_project", "title")),
        spaced(get_copy("select_project", "text")),
        shinyWidgets::pickerInput(
          ns("project"),
          label = NULL,
          multiple = TRUE,
          choices = projects,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            size = 10,
            maxOptions = 1,
            noneSelectedText = get_copy("select_project", "placeholder")
          )
        )
      )
    })

    shiny::observe({
      # Update r$project with selected project ----
      r$project <- input$project

      r$project_name <- r$projects %>%
        dplyr::filter(id == r$project) %>%
        dplyr::pull(name)

      # Use "me" endpoint to check if they are an admin for the project
      r$project_role <- r$me %>%
        dplyr::pull(projects) %>%
        purrr::pluck(1) %>%
        dplyr::filter(id == r$project) %>%
        dplyr::pull(role)

      r$is_project_admin <- r$project_role == "Admin"

      if (!r$is_project_admin) {
        show_not_project_admin_modal(r)
        r$step_select_valid_project_done <- FALSE
      } else {
        r$step_select_valid_project_done <- TRUE
      }
    }) %>%
      shiny::bindEvent(input$project)

    # Disable project selection once data is uploaded and valid
    shiny::observe({
      shiny::req(r$ready_to_map_aux)
      disable_picker_input(ns("project"))
    }) %>%
      shiny::bindEvent(r$ready_to_map_aux)

    # Reset, re-enable, and hide project selection on refresh ----
    shiny::observe({
      enable_picker_input(ns("project"))

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project",
        selected = character(0)
      )

      r$is_project_admin <- FALSE

      # Hide project selection
      shinyjs::hide(selector = ".project-selection")
    }) %>%
      shiny::bindEvent(r$reset)
  })
}

# Utils ----

show_not_project_admin_modal <- function(r) {
  project_role <- r$project_role

  cat("Not admin \n")
  text <- skeleton_to_text(get_copy("select_project", "not_admin"), list(project_name = r$project_name, project_id = r$project, role = project_role))
  show_modal(text)
}
