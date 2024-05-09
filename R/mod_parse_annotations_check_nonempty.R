#' parse_annotations_check_nonempty UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_check_nonempty_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' parse_annotations_check_nonempty Server Functions
#'
#' @noRd
mod_parse_annotations_check_nonempty_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check that Date, Site, Management, and Transect Number are not empty
    shiny::observe({
      # Iterate through and check if any values are empty
      check_fields <- append(get_config("additional_columns_map"), r$auxiliary_columns_map)

      empty_fields <- purrr::map(
        check_fields,
        \(x) {
          # Get values for field
          values <- r$annotations[x$column]
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
        empty_fields_skeleton <- get_copy("non_empty_fields")
        empty_fields_list <- make_formatted_list(empty_fields)

        all_fields <- check_fields %>%
          purrr::map("label") %>%
          glue::glue_collapse(sep = ", ", last = ", and ")

        empty_fields_glue <- list(fields = all_fields, list = empty_fields_list)

        empty_fields_text <- skeleton_to_text(empty_fields_skeleton, empty_fields_glue)

        show_modal(empty_fields_text)
      }
    }) %>%
      shiny::bindEvent(r$aux_mapped)
  })
}

## To be copied in the UI
# mod_parse_annotations_check_nonempty_ui("parse_annotations_check_nonempty_1")

## To be copied in the server
# mod_parse_annotations_check_nonempty_server("parse_annotations_check_nonempty_1")
