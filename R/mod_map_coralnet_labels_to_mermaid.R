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

      r$annotations["Label"] %>%
        dplyr::rename(coralnet_label = Label) %>%
        dplyr::arrange(coralnet_label) %>%
        dplyr::distinct()
    })

    # Check uploaded mapping (r$coralnet_upload) against `coralnet_mermaid_attributes` ----
    coralnet_mermaid_mapping <- shiny::reactive({
      shiny::req(r$all_aux_fields_valid)

      # Create table of mapping that does exist by left joining annotations' labels to the known mapping
      annotations_labels() %>%
        dplyr::left_join(known_mapping(), by = "coralnet_label")
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

      coralnet_mermaid_mapping() %>%
        rhandsontable::rhandsontable(
          rowHeaders = FALSE, # Remove row numbers
          contextMenu = FALSE, # Disable right clicking
          overflow = "visible", # So dropdown can extend out of table
        ) %>%
        # Make the coralnet label read only
        rhandsontable::hot_col("coralnet_label", readOnly = TRUE) %>%
        # Validator is not working - but can potentially use a custom renderer to make a cell red if it needs to be validated?
        # Also, should there be a column to reset the mapping to original? if they changed it?
        # Validate there are no non-NA values of `mermaid_attribute` - turn them red to indicate they need to be filled in
        # rhandsontable::hot_validate_character("mermaid_attribute", benthic_attribute_levels, allowInvalid = FALSE) %>%
        # rhandsontable::hot_validate_character("mermaid_growth_form", growth_form_levels, allowInvalid = TRUE) %>%
        # Make the mermaid_attribute col editable, with dropdown of options from the mapping table
        rhandsontable::hot_col("mermaid_attribute",
          type = "dropdown",
          source = benthic_attribute_levels,
          strict = TRUE,
          allowInvalid = FALSE
        ) %>%
        rhandsontable::hot_col("mermaid_growth_form",
          type = "dropdown",
          source = growth_form_levels,
          strict = TRUE,
          allowInvalid = FALSE
        )
    })
    # The flow is:
    # Show the mapping that there is, but make the MERMAID attribute editable if that's not what they want to map it to
    # If there is no mapping, make them select from a MERMAID attribute

    # Should there be an option at this point to save the data with the MERMAID attribute - so if they re-upload later on, they won't have to go through this process again? And it could check for MERMAID specific columns? Is that too complicated?

    # Put the editable table in a modal ----
    shiny::observe({
      shiny::req(coralnet_mermaid_mapping())

      confirm_modal(
        rhandsontable::rHandsontableOutput(ns("mapping_table")),
        footer_id = ns("save_mapping")
      )
    })

    # Enable closing of mapping widget ----
    # Closing disabled unless all of `mermaid_attribute` are not NA
    # If none of `mermaid_attribute` are NA, then enable exiting the widget
    # Flag that the mapping is valid, and save the final mapping
    shiny::observe({
      # The data in the table is named after the output, so it's input$mapping_table
      # Need to convert it to an R data frame using rhandsontable::hot_to_r()
      shiny::req(input$mapping_table)

      edited_coralnet_mermaid_mapping <- rhandsontable::hot_to_r(input$mapping_table)

      mapping_valid <- edited_coralnet_mermaid_mapping %>%
        dplyr::filter(is.na(mermaid_attribute)) %>%
        nrow() == 0

      if (mapping_valid) {
        shinyjs::enable("save_mapping")
        r$mapping_valid <- TRUE
        r$coralnet_mermaid_mapping <- edited_coralnet_mermaid_mapping
      } else {
        shinyjs::disable("save_mapping")
      }
    })
  })
}
