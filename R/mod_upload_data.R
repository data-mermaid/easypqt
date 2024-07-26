#' upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_upload_data_ui <- function(id) {
  ns <- NS(id)

  shiny::uiOutput(ns("upload"))
}

#' upload_data Server Functions
#'
#' @noRd
mod_upload_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show upload form once confirmed they are a project admin (they have selected a valid project), and additionally on any reset ----
    shiny::observe({
      output$upload <- renderUI({
        if (r$step_select_valid_project_done) {
          shiny::div(
            id = "upload-parent",
            shiny::h2(get_copy("upload_data", "title")),
            shiny::div(get_copy("upload_data", "text")),
            shiny::fileInput(ns("annotations"),
              label = NULL,
              accept = ".csv"
            )
          )
        } else {
          shiny::tagList()
        }
      })
    }) %>%
      shiny::bindEvent(r$step_select_valid_project_done, r$reset)

    # Check the file contains the correct columns ----
    shiny::observe({
      cols <- readr::read_csv(input$annotations$datapath, n_max = 0, show_col_types = FALSE)

      # Check if it is semicolon separated
      if (ncol(cols) == 1 & all(stringr::str_detect(names(cols), ";"))) {
        r$csv_sep <- ";"
        cols <- readr::read_delim(input$annotations$datapath, n_max = 0, show_col_types = FALSE, delim = r$csv_sep)
      } else {
        r$csv_sep <- ","
      }

      cols <- names(cols)

      # There are required columns, but the auxiliary columns do not actually have to be named Aux1, Aux2, ..., Aux 5
      # So look for columns in `required_annotations_columns_start`, then # of columns in `required_annotations_columns_aux`, then `required_annotations_columns_end`

      required_annotations_columns_start <- get_config("required_annotations_columns_start")
      required_annotations_columns_end <- get_config("required_annotations_columns_end")

      contains_known_required_columns <- all(c(required_annotations_columns_start, required_annotations_columns_end) %in% cols)

      if (contains_known_required_columns) {
        last_start_col <- required_annotations_columns_start[length(required_annotations_columns_start)]
        last_start_col_index <- which(cols == last_start_col)
        first_end_col <- required_annotations_columns_end[1]
        first_end_col_index <- which(cols == first_end_col)

        potential_aux_columns <- cols[(last_start_col_index + 1):(first_end_col_index - 1)]
        contains_n_aux_columns <- length(potential_aux_columns) == get_config("required_annotations_columns_aux")

        if (contains_known_required_columns & contains_n_aux_columns) {
          r$upload_contains_required_cols <- TRUE
          r$auxiliary_columns <- potential_aux_columns
          r$required_annotations_columns <- c(required_annotations_columns_start, r$auxiliary_columns, required_annotations_columns_end)
        }
      }

      # If it does not contain the correct columns, show a modal and do not allow them to continue
      if (!r$upload_contains_required_cols) {
        shiny::showModal(
          shiny::modalDialog(
            shiny::div(get_copy("upload_data", "missing_instructions")),
            shiny::tags$img(
              src = get_config("upload_data_missing_img_path"),
              alt = get_copy("upload_data", "missing_img_alt"),
              style = "width: 100%"
            ),
            footer = close_button,
            size = "m",
            easyClose = TRUE
          )
        )
      } else {
        # If it does contain the correct columns, read in the data and proceed
        # Only read in the required columns
        r$annotations_raw <- readr::read_delim(input$annotations$datapath, show_col_types = FALSE, col_select = r$required_annotations_columns, delim = r$csv_sep)

        # Disable data upload after a single upload - need to reset to change data
        shinyjs::disable("annotations")

        # Pointer etc of disabling
        # Disable pointer events on actual button, add style
        # Not allowed cursor on parent div, add style
        shinyjs::runjs("document.getElementById('upload-parent').getElementsByClassName('input-group')[0].style.pointerEvents = 'none'; document.getElementById('upload-parent').style.cursor = 'not-allowed';")

        # Flag that valid data has been uploaded
        r$step_upload_valid_data_done <- TRUE
      }
    }) %>%
      shiny::bindEvent(input$annotations)
  })
}
