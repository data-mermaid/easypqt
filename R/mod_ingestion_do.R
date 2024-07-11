#' ingestion_do UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ingestion_do_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' ingestion_do Server Functions
#'
#' @noRd
mod_ingestion_do_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      ingestion_data <- r$ingestion_data_with_defaults
      ingestion_data %>% dplyr::select(-`Sample date: Year *`) -> ingestion_data_error

      # Do dry run, handle errors if they come up -----

      dry_run_success <- ingest_and_handle_errors(ingestion_data, r$project, r$mermaidr_token, dryrun = TRUE)

      # Do actual import if no errors in dry run ----

      shiny::req(dry_run_success)

      import_success <- ingest_and_handle_errors(ingestion_data, r$project,  r$mermaidr_token, dryrun = FALSE)

      # If actual import is successful, show a modal with this information, and for them to go into Collect and validate/submit -----

      shiny::req(import_success)

      collect_url_skeleton <- ifelse(r$prod, get_copy("ingestion_success", "collect_url_prod"), get_copy("ingestion_success", "collect_url_dev"))
      collect_url <- glue::glue(collect_url_skeleton, .envir = list(project = r$project))

      show_modal(
        title = get_copy("ingestion_success", "title"),
        shiny::div(shiny::HTML(get_copy("ingestion_success", "text"))),
        success_button(
          ns("go_to_mermaid"),
          get_copy("ingestion_success", "button"),
          onclick = glue::glue("window.open('{link}', '_blank')", link = collect_url)
        ) %>%
          shiny::div(class = "space")
      )
    }) %>%
      shiny::bindEvent(r$do_ingestion)
  })
}

ingest_and_handle_errors <- function(data, project, token, dryrun) {
  res <- mermaidr::mermaid_import_project_data(data, project, "benthicpqt", dryrun = dryrun, token = token) %>%
    tryCatch(error = function(e) e)

  res_error <- res$message

  ## If not successful, show error message/to contact us -----

  res_success <- is.null(res_error)

  if (!res_success) {
    # Generate message for modal
    modal_message <- skeleton_to_text(get_copy("ingestion_error", "error"), .envir = list(project = project, error = res_error))

    # Show modal
    show_modal(
      title = get_copy("ingestion_error", "title"),
      shiny::div(shiny::HTML(get_copy("ingestion_error", "text"))),
      shiny::div(class = "error", modal_message),
      ## TODO -> download data again?
    )
  }

  # Return results
  res_success
}

## To be copied in the UI
# mod_ingestion_do_ui("ingestion_do_1")

## To be copied in the server
# mod_ingestion_do_server("ingestion_do_1")
