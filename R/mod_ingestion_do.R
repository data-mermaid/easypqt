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
  tagList(
    mod_reset_ui(ns("ingestion_reset"), show_ui = FALSE)
  )
}

#' ingestion_do Server Functions
#'
#' @noRd
mod_ingestion_do_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      shiny::req(r$do_ingestion)

      ingestion_data <- r$ingestion_data_with_defaults

      # Do dry run, handle errors if they come up -----

      dry_run_success <- ingest_and_handle_errors(ingestion_data, r$project, r$mermaidr_token, dryrun = TRUE)

      # Do actual import if no errors in dry run ----
      if (dry_run_success[["success"]]) {
        import_success <- ingest_and_handle_errors(ingestion_data, r$project, r$mermaidr_token, dryrun = FALSE)
      } else {
        import_success <- dry_run_success
      }

      # If actual import is successful, show a modal with this information, and for them to go into Collect and validate/submit -----
      # Otherwise, show the error - either the dry run error, or the non-dry run error if dry run was successful but the actual import was not

      if (import_success[["success"]]) {
        collect_url_skeleton <- ifelse(r$prod, get_copy("ingestion_success", "collect_url_prod"), get_copy("ingestion_success", "collect_url_dev"))
        collect_url <- glue::glue(collect_url_skeleton, .envir = list(project = r$project))

        modal_title <- title <- get_copy("ingestion_success", "title")

        modal_content <- shiny::tagList(
          shiny::div(shiny::HTML(get_copy("ingestion_success", "text"))),
          success_button(
            ns("go_to_mermaid"),
            get_copy("ingestion_success", "button"),
            onclick = glue::glue("window.open('{link}', '_blank')", link = collect_url)
          ) %>%
            shiny::div(class = "space")
        )
      } else {
        modal_title <- get_copy("ingestion_error", "title")
        modal_content <- shiny::tagList(
          shiny::div(shiny::HTML(get_copy("ingestion_error", "text"))),
          shiny::div(class = "error", skeleton_to_text(get_copy("ingestion_error", "error"), list(project = r$project, error = import_success[["error"]])))
        )
      }
      modal_close <- button(ns("ingestion_close"), "Close") # TODO copy

      cat("Ingestion done \n")
      show_modal(
        title = modal_title,
        modal_content,
        footer = modal_close
      )
    }) %>%
      shiny::bindEvent(r$do_ingestion)

    shiny::observe({
      shiny::removeModal()
      mod_reset_server("ingestion_reset", r, show_ui = FALSE, show_confirm = FALSE)
    }) %>%
      shiny::bindEvent(input$ingestion_close)
  })
}

safely_import_project_data <- purrr::safely(mermaidr::mermaid_import_project_data)

ingest_and_handle_errors <- function(data, project, token, dryrun) {
  res <- safely_import_project_data(data, project, "benthicpqt", dryrun = dryrun, token = token)

  # Error can come from res$error, or the contents of res itself
  no_error <- is.null(res$error)
  no_result <- is.null(res$result)

  res_success <- no_error & no_result

  ## If not successful, show error message/to contact us -----
  if (!res_success) {
    if (!no_error) {
      ingest_error <- res$error
    } else {
      ingest_error <- res$result

      # Format
      t <- tempfile(fileext = ".txt")
      sink(t)
      for (i in 1:nrow(ingest_error)) {
        print(as.list(ingest_error[i, ]))
      }
      sink()
      ingest_error <- readLines(t) %>% paste0(collapse = "<br>")
    }
  } else {
    ingest_error <- NULL
  }

  list(
    success = res_success,
    error = ingest_error
  )
}

## To be copied in the UI
# mod_ingestion_do_ui("ingestion_do_1")

## To be copied in the server
# mod_ingestion_do_server("ingestion_do_1")
