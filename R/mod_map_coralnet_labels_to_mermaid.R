#' mod_map_coralnet_labels_to_mermaid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_coralnet_labels_to_mermaid_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList()
}

#' mod_map_coralnet_labels_to_mermaid Server Functions
#'
#' @noRd
mod_map_coralnet_labels_to_mermaid_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    known_mapping <- shiny::reactive({
      shiny::req(r$all_aux_fields_valid)

      # Get known mapping from endpoint
      # TODO, actually get from endpoint
      easypqt::coralnet_mermaid_attributes
    })

    benthic_attributes <- shiny::reactive({
      shiny::req(r$all_aux_fields_valid)

      # Get other benthic attributes (to show *not* known mappings in dropdown)
      mermaidr::mermaid_get_reference("benthicattributes") %>%
        dplyr::filter(status == "Open") %>% # TODO? Ask Kim
        dplyr::pull(name)
    })

    growth_forms <- shiny::reactive({
      shiny::req(r$all_aux_fields_valid)

      growth_forms <- mermaidr::mermaid_get_endpoint("choices") %>%
        dplyr::filter(name == "growthforms") %>%
        dplyr::pull(data)

      growth_forms[[1]][["name"]]
    })

    annotations_labels <- shiny::reactive({
      shiny::req(r$all_aux_fields_valid)

      col <- get_config("coralnet_labelset_column")[["coralnet_col"]]

      r$annotations[col] %>%
        dplyr::distinct()
    })

    # Check uploaded mapping (r$coralnet_upload) against `coralnet_mermaid_attributes` ----
    coralnet_mermaid_mapping <- shiny::reactive({
      shiny::req(r$all_aux_fields_valid)

      coralnet_col <- get_config("coralnet_labelset_column")[["coralnet_col"]]
      mermaid_col <- get_config("coralnet_labelset_column")[["mermaid_join"]]

      # Create table of mapping that does exist by left joining annotations' labels to the known mapping

      annotations_labels() %>%
        dplyr::left_join(known_mapping(), by = setNames(mermaid_col, coralnet_col)) %>%
        # Put blanks first, then arrange alphabetically
        dplyr::mutate(.is_na = is.na(mermaid_attribute)) %>%
        dplyr::arrange(
          dplyr::desc(.is_na),
          !!rlang::sym(coralnet_col)
        ) %>%
        dplyr::select(-.is_na)
    }) %>%
      shiny::bindEvent(r$all_aux_fields_valid)

    # Create an editable table to be shown -----
    output$mapping_table <- rhandsontable::renderRHandsontable({
      # List of possible dropdown values for benthic attribute and growth form

      # For benthic attribute, the levels are the known mapping + anything in `benthic_attributes` that isn't in the known mapping
      benthic_attribute_levels <- c(known_mapping()[["mermaid_attribute"]], benthic_attributes()) %>%
        unique() %>%
        sort()

      # For growth form, it's `growth_forms`
      growth_form_levels <- growth_forms() %>% sort()

      coralnet_label_display <- get_config("coralnet_labelset_column")[["table_label"]]
      mermaid_benthic_attribute_display <- get_config("mermaid_attributes_columns")[["mermaid_attribute"]][["table_label"]]
      mermaid_growth_form_display <- get_config("mermaid_attributes_columns")[["mermaid_growth_form"]][["table_label"]]

      coralnet_mermaid_mapping() %>%
        rhandsontable::rhandsontable(
          rowHeaders = FALSE, # Remove row numbers
          contextMenu = FALSE, # Disable right clicking
          overflow = "visible", # So dropdown can extend out of table
          stretchH = "all",
          colHeaders = c(coralnet_label_display, mermaid_benthic_attribute_display, mermaid_growth_form_display)
        ) %>%
        # Make the coralnet label read only
        rhandsontable::hot_col(coralnet_label_display, readOnly = TRUE) %>%
        # Enable column sorting
        rhandsontable::hot_cols(columnSorting = TRUE) %>%
        # Highlight cells that need to be filled out %>%
        rhandsontable::hot_col(mermaid_benthic_attribute_display, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value === null) {
              td.style.background = 'pink';
             }
           }") %>%
        rhandsontable::hot_col(mermaid_benthic_attribute_display,
          type = "dropdown",
          # type = "autocomplete",
          source = benthic_attribute_levels,
          strict = TRUE
        ) %>%
        rhandsontable::hot_col(mermaid_growth_form_display,
          # type = "autocomplete",
          type = "dropdown",
          source = c(NA_character_, growth_form_levels), # To allow it to be empty?
          strict = TRUE
        )
    })
    # The flow is:
    # Show the mapping that there is, but make the MERMAID attribute editable if that's not what they want to map it to
    # If there is no mapping, make them select from a MERMAID attribute

    # Should there be an option at this point to save the data with the MERMAID attribute - so if they re-upload later on, they won't have to go through this process again? And it could check for MERMAID specific columns? Is that too complicated?

    # Put the editable table in an accordion ----
    shiny::observe({
      shiny::req(coralnet_mermaid_mapping())

      r$accordion_map_coralnet_labels <- bslib::accordion_panel(
        title = shiny::h2("Map CoralNet Labels to MERMAID Attributes"),
        value = "map-coralnet-labels",
        indent(
          rhandsontable::rHandsontableOutput(ns("mapping_table")) %>%
            shinycssloaders::withSpinner(),
          shinyjs::disabled(confirm_button(ns("save_mapping"))),
          shinyjs::disabled(shiny::actionButton(ns("edit"), "Edit"))
        )
      )
    })

    edited_coralnet_mermaid_mapping <- shiny::reactive({
      shiny::req(input$mapping_table)
      rhandsontable::hot_to_r(input$mapping_table)
    })

    # Enable confirming of mapping widget ----
    # Closing disabled unless all of `mermaid_attribute` are not NA
    # If none of `mermaid_attribute` are NA, then enable exiting the widget
    # Flag that the mapping is valid, and save the final mapping
    shiny::observe({
      # The data in the table is named after the output, so it's input$mapping_table
      # Need to convert it to an R data frame using rhandsontable::hot_to_r()

      mapping_valid <- edited_coralnet_mermaid_mapping() %>%
        dplyr::filter(is.na(mermaid_attribute)) %>%
        nrow() == 0

      if (mapping_valid) {
        shinyjs::enable("save_mapping")
        r$coralnet_mapping_valid <- TRUE
      } else {
        shinyjs::disable("save_mapping")
        r$coralnet_mapping_valid <- FALSE
        r$coralnet_mermaid_mapping <- NULL
      }
    })

    # When the label mapping has been confirmed ----
    shiny::observe({
      if (r$dev) {
        shiny::req(r$auxiliary_columns_map$site$value)
        r$confirm_map_aux_fields <- TRUE
      } else {
        r$confirm_map_aux_fields <- input$confirm
      }
      r$coralnet_mermaid_mapping <- edited_coralnet_mermaid_mapping()
      r$coralnet_labels_on_edit <- TRUE
      # Disable confirm, enable "edit"
      shinyjs::disable("save_mapping")
      shinyjs::enable("edit")
      # TODO disable the table
    }) %>%
      shiny::bindEvent(input$save_mapping)

    # Re-enable table when "edit" is clicked
    shiny::observe({
      shinyjs::disable("edit")
      shinyjs::enable("save_mapping")
      # TODO Enable table again
    }) %>%
      shiny::bindEvent(input$edit)

    # Create a new version of the annotations with the mapping ----
    shiny::observe({
      shiny::req(r$coralnet_mermaid_mapping)

      mermaid_attributes_cols <- get_config("mermaid_attributes_columns") %>%
        purrr::map("column")
      mermaid_attributes_cols <- setNames(names(mermaid_attributes_cols), mermaid_attributes_cols)

      r$annotations_mapped <- r$annotations %>%
        dplyr::left_join(r$coralnet_mermaid_mapping, get_config("coralnet_labelset_column")[["coralnet_col"]]) %>%
        dplyr::rename(mermaid_attributes_cols)
    })
  })
}
