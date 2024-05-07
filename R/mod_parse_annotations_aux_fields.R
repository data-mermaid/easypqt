#' parse_annotation_aux_fields UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_aux_fields_ui <- function(id) {
  ns <- NS(id)
  shiny::column(width = 6, shiny::uiOutput(ns("map_aux_fields")))
}

#' parse_annotations_aux_fields Server Functions
#'
#' @noRd
mod_parse_annotations_aux_fields_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Once the annotations file has been verified to contain the correct fields, need to determine which of the auxiliary fields contain Site, Management, and Transect Number

    # Use a dropdown for each rather than radio buttons - just a bit more complicated to disable etc and to work within a data table

    # It would be nice to allow them to optionally preview the data in here to remind themselves which of the auxliary columns is which?

    # Generate input UI ----
    output$map_aux_fields <- shiny::renderUI({
      shiny::req(r$annotations)

      purrr::imap(
        r$auxiliary_columns_map,
        \(x, y) {
          shiny::fluidRow(
            shiny::column(
              width = 6,
              # TODO: Vertically align with input
              shiny::tags$b(x)
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns(y),
                label = NULL,
                choices = r$auxiliary_columns,
                multiple = TRUE,
                # TODO, CSS styling for this to look like single selection, e.g. darker highlighting and not a check mark
                options = shinyWidgets::pickerOptions(
                  maxOptions = 1,
                  noneSelectedText = "Select an auxiliary field"
                )
              )
            )
          )
        }
      )
    })

    # Observe each dropdown, and disable an Aux field in other dropdowns if it's already selected - because an auxiliary field cannot map to more than one of Site, Management, or Transect Number

    # Update list of mapped columns
    purrr:::walk(
      names(get_config("auxiliary_columns_map")),
      \(x)
      shiny::observeEvent(input[[x]], {
        r$auxiliary_columns_mapping[[x]] <- input[[x]]
      })
    )

    # Go through each and disable other columns' aux fields
    shiny::observeEvent(r$auxiliary_columns_mapping,
      ignoreInit = TRUE,
      {
        # Go through each, and disable the other selected options
        purrr::walk(
          names(r$auxiliary_columns_mapping),
          \(x) {
            disable_options <- r$auxiliary_columns_mapping[names(r$auxiliary_columns_mapping) != x] %>%
              purrr::compact() %>%
              unlist(use.names = FALSE)
            disabled_options <- r$auxiliary_columns %in% disable_options


            shinyWidgets::updatePickerInput(session, x,
              choices = r$auxiliary_columns,
              selected = input[[x]],
              choicesOpt = list(
                disabled = disabled_options,
                style = ifelse(disabled_options,
                  yes = "color: rgba(119, 119, 119, 0.5);",
                  no = ""
                )
              )
            )
          }
        )
      }
    )
  })
}

## To be copied in the UI
# mod_parse_annotations_ui("parse_annotations_1")

## To be copied in the server
# mod_parse_annotations_server("parse_annotations_1")
