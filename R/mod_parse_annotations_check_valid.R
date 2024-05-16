#' parse_annotations_check_valid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_check_valid_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList()
}

#' parse_annotations_check_valid Server Functions
#'
#' @noRd
mod_parse_annotations_check_valid_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate fields ----
    # This needs to happen any time the mapping is edited, not just when it's confirmed the first time
    shiny::observe({
      shiny::req(r$aux_mapped > 0)
      shiny::req(r$no_empty_fields)

      # Check that sites are ones already entered in the project ----
      site_ui <- check_valid_values(r, "site")

      # Check that managements are ones already entered in the project ----
      management_ui <- check_valid_values(r, "management")

      # Check that transect number is an integer ----
      transect_number_ui <- check_integer_values(r, "transect_number")

      if (site_ui[["valid"]] & management_ui[["valid"]] & transect_number_ui[["valid"]]) {
        r$all_aux_fields_valid <- TRUE
      } else {
        r$all_aux_fields_valid <- FALSE

        show_modal(
          title = "Validating fields",
          site_ui[["ui"]],
          management_ui[["ui"]],
          transect_number_ui[["ui"]],
          "Please fix invalid values in CoralNet before continuing."
        )
      }
    }) %>%
      shiny::bindEvent(r$aux_mapped)
  })
}

## To be copied in the UI
# mod_parse_annotations_check_valid_ui("parse_annotations_check_valid_1")

## To be copied in the server
# mod_parse_annotations_check_valid_server("parse_annotations_check_valid_1")

check_valid_values <- function(r, lookup) {
  options_lookup <- r$auxiliary_columns_map[[lookup]][["column"]]

  valid_values <- r$template_choices[[options_lookup]][["value"]] # TODO, "value" in config?
  actual_values <- unique(r$annotations[[options_lookup]])

  invalid_values <- setdiff(actual_values, valid_values)

  all_valid <- length(invalid_values) == 0

  if (!all_valid) {
    invalid_values_skeleton <- get_copy("invalid_values")
    invalid_values <- make_formatted_list(invalid_values)
    valid_values <- make_formatted_list(valid_values)

    invalid_values_envir <- list(
      label = r$auxiliary_columns_map[[lookup]][["label"]],
      invalid_values = invalid_values,
      valid_values = valid_values
    )

    res <- skeleton_to_text(invalid_values_skeleton, invalid_values_envir)
  } else {
    res <- glue::glue("{lookup} values good")
    # TODO
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(r$auxiliary_columns_map[[lookup]][["label"]]),
        res
      )
  )
}

check_integer_values <- function(r, lookup) {
  values <- r$annotations[r$auxiliary_columns_map[[lookup]][["column"]]]
  names(values) <- "value"

  values_with_numeric <- values %>%
    dplyr::mutate(numeric_value = as.integer(value) %>% suppressWarnings())

  invalid_values <- values_with_numeric %>%
    dplyr::filter(is.na(numeric_value) |
      as.character(numeric_value) != value) # Handles decimals

  all_valid <- nrow(invalid_values) == 0

  if (!all_valid) {
    # Show the invalid ones only

    transect_number_invalid_skeleton <- get_copy("transect_number_not_integer")
    invalid_values <- make_formatted_list(invalid_values[["value"]])

    res <- skeleton_to_text(transect_number_invalid_skeleton, list(invalid_values = invalid_values))
  } else {
    res <- glue::glue("{lookup} numbers good")
    # TODO
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(r$auxiliary_columns_map[[lookup]][["label"]]),
        res
      )
  )
}
