#' upload_annotations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_upload_annotations_ui <- function(id) {
  ns <- NS(id)

  shinyjs::hidden(
    shiny::fileInput(ns("annotations"),
      label = shiny::h2("Upload CoralNet annotations"),
      accept = ".csv"
    )
  )
}

#' upload_annotations Server Functions
#'
#' @noRd
mod_upload_annotations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show upload form once confirmed they are a project admin
    shiny::observe({
      req(r$is_project_admin)

      shinyjs::show("annotations")
    })

    # Check the file contains the correct columns
    shiny::observe({
      cols <- readr::read_csv(input$annotations$datapath, n_max = 0, show_col_types = FALSE)
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
          r$contains_required_cols <- TRUE
          r$auxiliary_columns <- potential_aux_columns
          r$required_annotations_columns <- c(required_annotations_columns_start, r$auxiliary_columns, required_annotations_columns_end)
        } else {
          r$does_not_contain_required_cols <- TRUE
        }
      } else {
        r$does_not_contain_required_cols <- TRUE
      }
    }) %>%
      shiny::bindEvent(input$annotations)

    # If it does not contain the correct columns, show a modal and do not allow them to continue
    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          shiny::div(get_copy("upload_annotations", "missing_instructions")),
          shiny::tags$img(
            src = get_config("upload_annotations_missing_img_path"),
            alt = get_copy("upload_annotations", "missing_img_alt"),
            style = "width: 100%"
          ),
          footer = close_button,
          size = "m",
          easyClose = TRUE
        )
      )
    }) %>%
      shiny::bindEvent(r$does_not_contain_required_cols)

    # If it does contain the correct columns, read in the data and proceed
    shiny::observe({
      shiny::req(r$is_project_admin)
      if (r$dev) {
        r$annotations_raw <- get(r$dev_scenario)
        r$ready_to_map_aux <- TRUE
        r$auxiliary_columns <- get_config("auxiliary_columns_dev")
        r$required_annotations_columns <- get_config("required_annotations_columns_dev")
      } else {
        shiny::req(r$contains_required_cols)
        # Only read in the required columns
        r$annotations_raw <- readr::read_csv(input$annotations$datapath, show_col_types = FALSE, col_select = r$required_annotations_columns)
        r$ready_to_map_aux <- TRUE

        # Disable data upload after a single upload - need to reset to change data
        shinyjs::disable("annotations")
      }
    })
  })
}
