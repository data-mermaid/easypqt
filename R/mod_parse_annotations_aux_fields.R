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
  # TODO - styling etc - in a modal?
  shiny::fluidRow(
  shiny::column(
    width = 6,
    shiny::uiOutput(ns("map_aux_fields"))
  )
  )
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

      inputs <- purrr::imap(
        # Just do this once, do not listen to changes in the mapping
        shiny::isolate(r$auxiliary_columns_map),
        \(x, y) {
          selected <- ifelse(r$dev, glue::glue("Aux{number}", number = which(names(shiny::isolate(r$auxiliary_columns_map)) == y)), NULL)
          shiny::fluidRow(
            shiny::column(
              width = 6,
              # TODO: Vertically align with input
              shiny::tags$b(x$label)
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns(y),
                label = NULL,
                choices = r$auxiliary_columns,
                selected = selected,
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

      shiny::tagList(
        shiny::h4("Map columns to auxiliary fields"),
        inputs,
        shinyjs::disabled(shiny::actionButton(ns("confirm"), "Confirm"))
      )
    })

    # Observe each dropdown, and disable an Aux field in other dropdowns if it's already selected - because an auxiliary field cannot map to more than one of Site, Management, or Transect Number

    # Update list of mapped columns ----
    purrr::walk(
      names(get_config("auxiliary_columns_map")),
      \(x)
      shiny::observe(priority = 1, {
        shiny::req(r$annotations)
        if (r$dev) {
          shiny::req(input$site)
        }
        r$auxiliary_columns_map[[x]]["value"] <- list(input[[x]])
        r$aux_mapping_ui_created <- TRUE
      })
    )

    # Go through each and disable other columns' aux fields ----
    shiny::observe({
      shiny::req(r$annotations)
      shiny::req(r$aux_mapping_ui_created)

      # Go through each, and disable the other selected options
      purrr::walk(
        names(r$auxiliary_columns_map),
        \(x) {
          disable_options <- r$auxiliary_columns_map[names(r$auxiliary_columns_map) != x] %>%
            purrr::map("value") %>%
            purrr::compact() %>%
            unlist(use.names = FALSE)
          disabled_options <- r$auxiliary_columns %in% disable_options
          shinyWidgets::updatePickerInput(
            session,
            x,
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
    })

    # Enable "confirm" button once all of the columns have been mapped to an auxiliary field ----
    shiny::observe({
      shiny::req(r$annotations)

      cols_mapped <- r$auxiliary_columns_map %>%
        purrr::map("value") %>%
        purrr::compact()

      all_cols_mapped <- length(cols_mapped) == length(r$auxiliary_columns_map)

      if (all_cols_mapped) {
        shinyjs::enable("confirm")
      } else {
        shinyjs::disable("confirm")
      }
    })

    shiny::observe({
      if (r$dev) {
        shiny::req(input$site)
        r$confirm_map_aux_fields <- TRUE
      } else {
        r$confirm_map_aux_fields <- input$confirm
      }
    })

    # Rename columns in data according to auxiliary fields mapping ----
    shiny::observe({

      mapped_cols_names <- r$auxiliary_columns_map %>%
        purrr::map("column")
      mapped_cols_aux <- r$auxiliary_columns_map %>%
        purrr::map("value")
      mapped_cols <- setNames(mapped_cols_aux, mapped_cols_names) %>%
        unlist()

      r$annotations <- r$annotations %>%
        dplyr::rename(mapped_cols)

      # Remove auxiliary fields that were not mapped
      extra_aux_fields <- setdiff(r$auxiliary_columns, mapped_cols_aux)

      r$annotations <- r$annotations %>%
        dplyr::select(-tidyselect::all_of(extra_aux_fields))

      r$aux_mapped <- TRUE
    }) %>%
      shiny::bindEvent(r$confirm_map_aux_fields)
  })
}

## To be copied in the UI
# mod_parse_annotations_ui("parse_annotations_1")

## To be copied in the server
# mod_parse_annotations_server("parse_annotations_1")
