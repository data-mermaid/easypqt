#' upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_upload_data_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("upload")),
    mod_upload_instructions_ui(ns("filetype"), show_ui = FALSE),
    mod_upload_instructions_ui(ns("zip"), show_ui = FALSE),
    mod_upload_instructions_ui(ns("cols"), show_ui = FALSE),
    mod_upload_instructions_ui(ns("date"), show_ui = FALSE)
  )
}

#' upload_data Server Functions
#'
#' @noRd
mod_upload_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show upload form once they have selected human or machine annotated (reefcloud),
    # or just once project admin / valid project confirmed (coralnet, but
    # set step_select_human_or_machine_annotated <- TRUE in mod_select_human_or_machine_annotated),
    # and additionally on any reset
    shiny::observe({
      shiny::req(r$step_select_human_or_machine_annotated)

      output$upload <- renderUI({
        if (r$step_select_human_or_machine_annotated) {
          shiny::div(
            id = "upload-parent",
            shiny::h2(get_copy("upload_data", "title", r$provider)),
            spaced(
              get_copy("upload_data", "text", r$provider),
              mod_upload_instructions_ui(ns("instructions")),
              shiny::HTML("</p>")
            ),
            shiny::fileInput(
              ns("annotations"),
              label = NULL,
              accept = get_config("upload_file")[[r$provider]]
            ),
            shiny::hr(),
            shiny::div()
          )
        } else {
          shiny::tagList()
        }
      })

      r$upload_form_done <- TRUE
    }) %>%
      shiny::bindEvent(r$step_select_human_or_machine_annotated, r$reset)

    # Scroll to upload form
    shiny::observe({
      shiny::req(r$upload_form_done)
      scroll_to_section("upload-parent")
    }) %>%
      shiny::bindEvent(r$upload_form_done)

    # Upload instructions ----
    mod_upload_instructions_server("instructions", r)

    shiny::observe({
      # For Reefcloud, it must be a zip--this is set in fileInput(), but they can still drag a non-zip file in
      # And for Coralnet, it must be a CSV
      # So check that the file type matches what is expected, otherwise error and do not allow them to continue

      file_type_matches <- stringr::str_ends(input$annotations$datapath, get_config("upload_file")[[r$provider]])
      if (!file_type_matches) {
        mod_upload_instructions_server("filetype", r, show_ui = FALSE, invalid = "invalid_filetype")
        r$annotations_upload_type_valid <- FALSE
      } else {
        r$annotations_upload_type_valid <- TRUE
      }
    }) %>%
      shiny::bindEvent(input$annotations)

    shiny::observe({
      shiny::req(r$annotations_upload_type_valid)

      # If the file type is zip, first unzip the file, check that it actually contains a csv, then read it in
      if (get_config("upload_file")[[r$provider]] == ".zip") {
        file_path <- input$annotations$datapath
        upload_dir <- stringr::str_remove(file_path, basename(file_path))

        # First, list the files
        zip_files <- unzip(file_path, list = TRUE)

        csv_files <- zip_files %>%
          # Ignore any subfolders, only at top level
          dplyr::filter(Name == basename(Name)) %>%
          # Check for a .csv file
          dplyr::filter(stringr::str_ends(Name, "csv"))

        # If no CSV, show instructions
        if (nrow(csv_files) != 1) {
          mod_upload_instructions_server("zip", r, show_ui = FALSE, invalid = "no_csv")
          r$annotations_upload_valid <- FALSE
        } else {
          # Otherwise, actually unzip and save the path
          r$annotations_path <- unzip(file_path, exdir = upload_dir, files = csv_files[["Name"]])
          r$annotations_upload_valid <- TRUE
        }
      } else {
        r$annotations_path <- input$annotations$datapath
        r$annotations_upload_valid <- TRUE
      }
    }) %>%
      shiny::bindEvent(r$annotations_upload_type_valid)

    # Check the file contains the correct columns ----
    shiny::observe({
      shiny::req(r$annotations_upload_valid)

      cols <- readr::read_csv(r$annotations_path, n_max = 0, show_col_types = FALSE)

      # Check if it is semicolon separated
      if (ncol(cols) == 1 & all(grepl(";", names(cols)))) {
        r$csv_sep <- ";"
        cols <- readr::read_delim(r$annotations_path, n_max = 0, show_col_types = FALSE, delim = r$csv_sep)
      } else {
        r$csv_sep <- ","
      }

      cols <- names(cols)

      provider <- r$provider

      # Required columns ----
      if (provider == "coralnet") {
        # There are required columns, but the auxiliary columns do not actually have to be named Aux1, Aux2, ..., Aux 5
        # So look for columns in `required_annotations_columns_start`, then # of columns in `required_annotations_columns_aux`, then `required_annotations_columns_end`

        required_annotations_columns_start <- get_config("required_annotations_columns_start")
        required_annotations_columns_end <- get_config("required_annotations_columns_end")

        contains_known_required_columns <- all(c(required_annotations_columns_start, required_annotations_columns_end) %in% cols)

        if (contains_known_required_columns) {
          last_start_col <- required_annotations_columns_start[length(required_annotations_columns_start)]
          last_start_col_index <- which(cols == last_start_col)
          first_aux_index <- last_start_col_index + 1
          last_aux_index <- last_start_col_index + get_config("required_annotations_columns_aux")

          r$auxiliary_columns <- cols[first_aux_index:last_aux_index]
          r$upload_contains_required_cols <- TRUE
          r$required_annotations_columns <- c(required_annotations_columns_start, r$auxiliary_columns, required_annotations_columns_end)
        } else {
          r$upload_contains_required_cols <- FALSE
        }
      } else if (provider == "reefcloud") {
        # Check that upload columns match the template columns (rather than listing all of the columns in the config file)
        reefcloud_template <- readr::read_csv(app_sys(get_config("reefcloud_template_path")), n_max = 0, show_col_types = FALSE)
        reefcloud_cols <- names(reefcloud_template)

        if (identical(cols, reefcloud_cols)) {
          r$upload_contains_required_cols <- TRUE
        } else {
          r$upload_contains_required_cols <- FALSE
        }
      }

      # If it does not contain the correct columns, show a modal and do not allow them to continue
      if (!r$upload_contains_required_cols) {
        mod_upload_instructions_server("cols", r, show_ui = FALSE, invalid = "missing_columns")
      } else {
        # If it does contain the correct columns, read in the data and proceed
        # Only read in the required columns

        if (provider == "coralnet") {
          annotations_raw <- readr::read_delim(input$annotations$datapath, show_col_types = FALSE, col_select = r$required_annotations_columns, delim = r$csv_sep)
        } else if (provider == "reefcloud") {
          annotations_raw <- readr::read_delim(r$annotations_path, show_col_types = FALSE, delim = r$csv_sep)

          # Filter only human annotated columns, if necessary
          if (r$human_annotated_only) {
            annotations_raw <- annotations_raw %>%
              dplyr::filter(!is.na(point_human_benthic_id))
          }
        }

        date_col <- get_config("provider_columns_date")[[r$provider]][["value"]]

        # Check that the Date column is formatted properly - if not, show a modal that there is an issue
        date_validation <- check_valid_dates(annotations_raw[[date_col]])
        if (!date_validation[["valid"]]) {
          mod_upload_instructions_server("date", r, show_ui = FALSE, invalid = "invalid_date")
        } else {
          r$annotations_raw <- annotations_raw
          # Reformat the dates to ymd
          r$annotations_raw[[date_col]] <- reformat_dates(annotations_raw[[date_col]], date_validation[["format"]])
        }

        # Disable data upload after a single upload - need to reset to change data
        shinyjs::disable("annotations")

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
      shiny::bindEvent(r$annotations_upload_valid)
  })
}

check_valid_dates <- function(dates) {
  dates <- unique(dates)

  # Allow for reformatting from excel -> check for ymd (coralnet version), then mdy, then dmy
  date_formats <- c("ymd", "ymd_hms", "mdy", "mdy_hms", "dmy", "dmy_hms", "ydm", "ydm_hms")

  invalid_dates <- TRUE

  i <- 1

  while (invalid_dates & i <= length(date_formats)) {
    format <- date_formats[[i]]
    invalid_dates <- do.call(eval(parse(text = glue::glue("lubridate::{format}"))), list(dates, quiet = TRUE)) %>%
      is.na() %>%
      any()
    i <- i + 1
  }

  if (invalid_dates & (i == length(date_formats) + 1)) {
    format <- NULL
  }

  list(
    valid = !invalid_dates,
    format = format
  )
}

reformat_dates <- function(dates, format) {
  dates <- do.call(eval(parse(text = glue::glue("lubridate::{format}"))), list(dates, quiet = TRUE))

  if (stringr::str_ends(format, "_hms")) {
    dates <- as.Date(dates)
  }

  dates
}
