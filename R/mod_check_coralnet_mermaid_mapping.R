#' check_coralnet_mermaid_mapping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_check_coralnet_mermaid_mapping_ui <- function(id) {
  ns <- NS(id)

  rhandsontable::rHandsontableOutput(ns("mapping_table"))
}

#' check_coralnet_mermaid_mapping Server Functions
#'
#' @noRd
mod_check_coralnet_mermaid_mapping_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check uploaded mapping (r$coralnet_upload) against `coralnet_mermaid_attributes` ----

    coralnet_mermaid_mapping <- shiny::reactive({
      shiny::req(r$is_project_admin)

      r$coralnet_upload %>%
        dplyr::left_join(coralnet_mermaid_attributes, by = "coralnet_label") %>%
        dplyr::select(coralnet_label, mermaid_attribute) %>%
        dplyr::mutate(mermaid_attribute = forcats::fct_expand(mermaid_attribute, coralnet_mermaid_attributes[["mermaid_attribute"]]))
      # Convert mermaid_attribute to a factor, because factors automatically get set to dropdown in rhandsontable, with the choices specified by level and allowInvalid set to FALSE
    }) %>%
      shiny::bindEvent(r$coralnet_upload, r$is_project_admin)

    output$mapping_table <- rhandsontable::renderRHandsontable({
      # Create an editable table to be shown
      coralnet_mermaid_mapping() %>%
        # coralnet_mermaid_mapping %>%
        rhandsontable::rhandsontable(
          rowHeaders = FALSE, # Remove row numbers
          contextMenu = FALSE # Disable right clicking
        ) %>%
        # Make the coralnet label read only
        rhandsontable::hot_col("coralnet_label", readOnly = TRUE)
      # Validator is not working - but can potentially use a custom renderer to make a cell red if it needs to be validated?
      # Also, should there be a column to reset the mapping to original? if they changed it?
      # Validate there are no non-NA values of `mermaid_attribute` - turn them red to indicate they need to be filled in
      # rhandsontable::hot_validate_character("mermaid_attribute", coralnet_mermaid_attributes[["mermaid_attribute"]], allowInvalid = FALSE)%>%
      # Make the mermaid_attribute col editable, with dropdown of options from the mapping table
      # rhandsontable::hot_col("mermaid_attribute",
      #   type = "dropdown",
      #   source = coralnet_mermaid_attributes[["mermaid_attribute"]],
      #   strict = TRUE,
      #   allowInvalid = FALSE
      # )
    })
    # The flow is:
    # Show the mapping that there is, but make the MERMAID attribute editable if that's not what they want to map it to
    # If there is no mapping, make them select from a MERMAID attribute

    # Should there be an option at this point to save the data with the MERMAID attribute - so if they re-upload later on, they won't have to go through this process again? And it could check for MERMAID specific columns? Is that too complicated?
  })
}
