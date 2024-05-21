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

  shiny::tagList(
    shinyjs::hidden(
      shinyWidgets::pickerInput(
        ns("project"),
        label = shiny::h2("Project"),
        multiple = TRUE,
        choices = NULL,
        options = shinyWidgets::pickerOptions(
          liveSearch = TRUE,
          size = 10,
          maxOptions = 1,
          noneSelectedText = "Select a project"
        )
      )
    )
  )
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

    # Update project selection dropdown based on user's projects ----

    shiny::observe({
      shiny::req(r$projects)

      projects <- setNames(r$projects$id, r$projects$name)

      selected_project <- null_if_dev(r$dev, "4d23d2a1-774f-4ccf-b567-69f95e4ff572")

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project", choices = projects, selected = selected_project
      )

      shinyjs::show("project")

      # shiny::tagList(
      #   indent(input)
      # )
    })

    shiny::observe({
      # Update r$project with selected project ----
      r$project <- input$project

      # Get project template/options ----
      # At this point, will get an error if they are not an admin
      template_and_options <- safely_get_template_and_options(input$project, "benthicpqt")

      r$is_project_admin <- check_project_admin(template_and_options)

      if (!r$is_project_admin) {
        show_not_project_admin_modal(r)
      } else {
        template_and_options <- template_and_options$result
        r$template <- template_and_options$Template
        r$template_choices <- template_and_options[names(template_and_options) != "Template"] %>%
          purrr::map("choices") %>%
          purrr::compact()
      }
    }) %>%
      shiny::bindEvent(input$project)
  })
}

# Utils ----

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

  show_modal(skeleton_to_text(get_copy("not_admin"), list(project_name = project_name)))
}
