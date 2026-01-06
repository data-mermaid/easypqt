#' check_valid_values UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_check_valid_values_ui <- function(id) {
  ns <- NS(id)
  # No UI for this, just modals
  tagList(
    mod_reset_ui(ns("reset"), show_ui = FALSE)
  )
}

#' check_valid_values Server Functions
#'
#' @noRd
mod_check_valid_values_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check that fields are not empty ----
    shiny::observe({
      shiny::req(r$step_fields_setup_done)

      # Check that Date, Site, Management, and Transect Number are not empty

      cat("Checking aux \n")
      show_modal(
        title = get_copy("auxiliary_validating", "title"),
        spaced(get_copy("auxiliary_validating", "checking")),
        footer = NULL
      )
      Sys.sleep(1)

      # Iterate through and check if any values are empty
      if (r$provider == "coralnet") {
        check_fields <- append(get_config(glue::glue("provider_additional_columns_map")), r$columns_map)
      } else if (r$provider == "reefcloud") {
        check_fields <- get_config("reefcloud_columns_map")
      }

      empty_fields <- purrr::map(
        check_fields,
        \(x) {
          # Get values for field
          values <- r$annotations_raw[x$value]
          names(values) <- "field"

          # Check if any are NA
          na_values <- values %>% dplyr::filter(is.na(field))
          any_na <- nrow(na_values) > 0

          # Return label of field if any are NA
          if (any_na) {
            x$label
          } else {
            # Otherwise, return NULL
            NULL
          }
        }
      ) %>%
        purrr::compact()

      if (length(empty_fields) > 0) {
        r$no_empty_fields <- FALSE
        empty_fields_skeleton <- get_copy("non_empty_fields", r$provider)
        empty_fields_list <- make_formatted_list(empty_fields)

        all_fields <- check_fields %>%
          purrr::map("label") %>%
          glue::glue_collapse(sep = ", ", last = ", and ")

        empty_fields_glue <- list(fields = all_fields, list = empty_fields_list)

        empty_fields_text <- skeleton_to_text(empty_fields_skeleton, empty_fields_glue)

        shiny::removeModal()

        cat("Empty aux \n")
        show_modal(
          title = get_copy("auxiliary_validating", "title"), empty_fields_text
        )
      } else {
        r$no_empty_fields <- TRUE
      }

      # Check valid values of fields ----
      # Validate fields ----
      shiny::req(r$no_empty_fields)

      # Get project template/options ----
      # Show modal that we are getting this?
      template_and_options <- mermaidr::mermaid_import_get_template_and_options(r$project, "benthicpqt", token = r$mermaidr_token)
      r$template <- template_and_options$Template
      r$template_choices <- template_and_options[names(template_and_options) != "Template"] %>%
        purrr::map("choices") %>%
        purrr::compact()

      # Check that sites are ones already entered in the project ----
      site_ui <- check_valid_values(r, "site")

      # Check that managements are ones already entered in the project ----
      management_ui <- check_valid_values(r, "management")

      # Check that transect number is an integer ----
      transect_number_ui <- check_integer_values(r, "transect_number")

      # Check depth is an integer ----
      if (r$provider == "reefcloud") {
        depth_ui <- check_integer_values(r, "depth")
      } else if (r$provider == "coralnet") {
        depth_ui <- list(valid = TRUE, ui = NULL)
      }

      # Hide checking modal
      shiny::removeModal()

      if (site_ui[["valid"]] & management_ui[["valid"]] & transect_number_ui[["valid"]] & depth_ui[["valid"]]) {
        r$step_map_auxiliary_fields_valid_done <- TRUE

        # Show modal that all is good
        # Show checking modal

        cat("All aux valid \n")
        show_modal(
          title = get_copy("auxiliary_validating", "title"),
          get_copy("auxiliary_validating", "all_valid", r$provider)
        )
      } else {
        cat("Issues with aux \n")
        show_modal(
          title = get_copy("auxiliary_validating", "title"),
          shiny::div(class = "validating-aux", get_copy("auxiliary_validating", "fix", r$provider)),
          shiny::hr(),
          site_ui[["ui"]],
          shiny::hr(),
          management_ui[["ui"]],
          shiny::hr(),
          transect_number_ui[["ui"]],
          {
            if (r$provider == "reefcloud") {
              tagList(
                shiny::hr(),
                depth_ui[["ui"]]
              )
            }
          },
          footer = warning_button(ns("incorrect_reset"), get_copy("ingestion", "reset_button"))
        )
      }
    })

    # Invalid -- do not require confirmation, since they can't proceed anyways
    shiny::observe({
      mod_reset_server("reset", r, show_ui = FALSE, show_confirm = FALSE)
    }) %>%
      shiny::bindEvent(input$incorrect_reset)
  })
}

## To be copied in the UI
# mod_check_valid_values_ui("check_valid_values_1")

## To be copied in the server
# mod_check_valid_values_server("check_valid_values_1")

check_valid_values <- function(r, lookup) {
  lookup <- r$columns_map[[lookup]]
  template_lookup <- lookup[["column"]]
  actual_lookup <- lookup[["value"]]

  valid_values <- r$template_choices[[template_lookup]][["value"]]
  actual_values <- unique(r$annotations_raw[[actual_lookup]])

  invalid_values <- setdiff(actual_values, valid_values)

  all_valid <- length(invalid_values) == 0

  column <- lookup[["label"]]

  if (!all_valid) {
    invalid_values_skeleton <- get_copy("invalid_values", r$provider)
    invalid_values <- make_formatted_list(invalid_values)
    valid_values <- make_formatted_list(valid_values)

    invalid_values_envir <- list(
      label = lookup[["label"]],
      invalid_values = invalid_values,
      valid_values = valid_values
    )

    if (r$provider == "reefcloud") {
      invalid_values_envir <- append(
        invalid_values_envir,
        list(provider_label = get_config("reshape")[["rename"]][["reefcloud"]][[lookup$column]])
      )
    }

    res <- skeleton_to_text(invalid_values_skeleton, invalid_values_envir)
  } else {
    valid_skeleton <- get_copy("auxiliary_validating", "column_valid", r$provider)
    valid_envir <- list(
      column = lookup[["label"]]
    )

    if (r$provider == "reefcloud") {
      valid_envir <- append(
        valid_envir,
        list(
          provider_column = get_config("reshape")[["rename"]][["reefcloud"]][[lookup$column]]
        )
      )
    }

    res <- skeleton_to_text(valid_skeleton, valid_envir)
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(column),
        shiny::div(class = "validating-aux", res)
      )
  )
}

check_integer_values <- function(r, lookup) {
  values <- r$annotations_raw[r$columns_map[[lookup]][["value"]]]
  column <- r$columns_map[[lookup]][["label"]]
  names(values) <- "value"

  values_with_numeric <- values %>%
    dplyr::mutate(numeric_value = as.integer(value) %>% suppressWarnings())

  invalid_values <- values_with_numeric %>%
    dplyr::filter(is.na(numeric_value) |
      as.character(numeric_value) != value) %>% # Handles decimals
    dplyr::distinct()

  all_valid <- nrow(invalid_values) == 0

  if (!all_valid) {
    # Show the invalid ones only
    invalid_skeleton <- get_copy("not_integer", lookup, r$provider)
    invalid_values <- make_formatted_list(invalid_values[["value"]])

    res <- skeleton_to_text(invalid_skeleton, list(invalid_values = invalid_values))
  } else {
    valid_skeleton <- get_copy("auxiliary_validating", "column_valid", r$provider)
    valid_envir <- list(
      column = lookup
    )

    if (r$provider == "reefcloud") {
      lookup_full_name <- r$columns_map[[lookup]][["column"]]
      valid_envir <- append(
        valid_envir,
        list(
          provider_column = get_config("reshape")[["rename"]][["reefcloud"]][[lookup_full_name]]
        )
      )
    }

    res <- skeleton_to_text(valid_skeleton, valid_envir)
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(r$columns_map[[lookup]][["label"]]),
        shiny::div(class = "validating-aux", res)
      )
  )
}
