#' upload_annotations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_upload_annotations_ui <- function(id) {
  ns <- NS(id)

  shiny::uiOutput(ns("upload"))
}

#' upload_annotations Server Functions
#'
#' @noRd
mod_upload_annotations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show upload form once confirmed they are a project admin
    output$upload <- shiny::renderUI({
      shiny::req(r$is_project_admin)

      shiny::fileInput(ns("annotations"), "Upload CoralNet annotations", accept = ".csv")
    })

    # Check the file contains the correct columns
    shiny::observe({
      cols <- readr::read_csv(input$annotations$datapath, n_max = 0, show_col_types = FALSE)
      cols <- names(cols)

      # Find columns that are required but not in data
      r$missing_cols <- setdiff(required_cols, cols)

      if (length(r$missing_cols) == 0) {
        r$contains_required_cols <- TRUE
      } else {
        r$does_not_contain_required_cols <- FALSE
      }
    }) %>%
      shiny::bindEvent(input$annotations)

    # If it does not contain the correct columns, show a modal and do not allow them to continue
    shiny::observe({
      missing_cols_list <- purrr::map(c(r$missing_cols), shiny::tags$li) %>%
        shiny::tags$ul()

      shiny::showModal(
        shiny::modalDialog(
          shiny::div(get_copy("upload_annotations_missing_pretext")),
          shiny::div(missing_cols_list),
          shiny::div(get_copy("upload_annotations_missing_instructions")),
          shiny::tags$img(
            src = get_config("upload_annotations_missing_img_path"),
            alt = get_copy("upload_annotations_missing_img_alt"),
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
      # Only read in the required columns
      r$annotations <- readr::read_csv(input$annotations$datapath, show_col_types = FALSE, col_select = required_cols)
    }) %>%
      shiny::bindEvent(r$contains_required_cols)


    # I don't have data yet - so I'll just fake a data set that has some right/wrong
    # shiny::observe({ # TODO - needs to depend on input$annotations later
    #   shiny::req(r$is_project_admin)
    #   r$project
    #
    #   r$coralnet_upload <- fake_data
    # })
  })
}

fake_data <- dplyr::tribble(
  ~coralnet_label,
  "psa",
  "Anem",
  "cyp",
  "SC",
  "por",
  "test"
)

# TODO - move to config? or at least data-raw
required_cols <- c(
  "Name", "Date", "Aux1", "Aux2", "Aux3", "Aux4", "Aux5", "Row",
  "Column", "Label"
)
