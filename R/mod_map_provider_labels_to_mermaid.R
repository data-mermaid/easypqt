#' mod_map_provider_labels_to_mermaid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_provider_labels_to_mermaid_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList()
}

#' mod_map_provider_labels_to_mermaid Server Functions
#'
#' @noRd
mod_map_provider_labels_to_mermaid_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    known_mapping <- shiny::reactive({
      # Get known mapping from endpoint
      # TODO -> once it includes reefcloud ones on dev
      # mermaidr::mermaid_get_classification_labelmappings(r$provider_full) %>%
      classification_labelmappings %>%
        dplyr::filter(provider == r$provider_full) %>%
        dplyr::select(dplyr::all_of(c(
          get_config("labelset_id_column")[["mermaid_join"]],
          get_config("mermaid_attributes_columns") %>% purrr::map_chr("api_column")
        )))
    })

    annotations_labels <- shiny::reactive({
      if (r$provider == "reefcloud") {
        provider_id_cols <- get_config("labelset_id_column")[["provider_col"]][[r$provider]]
        provider_id <- names(provider_id_cols)
        provider_id_cols <- provider_id_cols[[provider_id]]

        provider_code_cols <- get_config("labelset_code_column")[["provider_col"]][[r$provider]]
        provider_code <- names(provider_code_cols)
        provider_code_cols <- provider_code_cols[[provider_code]]

        r$annotations <- r$annotations %>%
          dplyr::mutate(
            "{provider_id}" := !!quote(dplyr::coalesce(!!!dplyr::across(provider_id_cols))),
            "{provider_code}" := !!quote(dplyr::coalesce(!!!dplyr::across(provider_code_cols)))
          ) %>%
          dplyr::select(-dplyr::all_of(c(provider_id_cols, provider_code_cols)))
      } else if (r$provider == "coralnet") {
        provider_id <- get_config("labelset_id_column")[["provider_col"]][[r$provider]]
        provider_code <- get_config("labelset_code_column")[["provider_col"]][[r$provider]]
        annotations <- r$annotations[c(provider_id, provider_code)]
      }

      annotations <- r$annotations[c(
        provider_id, provider_code
      )]

      r$provider_col_id <- provider_id
      r$provider_col_code <- provider_code

      annotations %>%
        dplyr::distinct() %>%
        dplyr::mutate(dplyr::across({{ provider_id }}, as.character))
    })

    # Check uploaded mapping (r$provider_upload) against `provider_mermaid_attributes` ----
    provider_mermaid_mapping <- shiny::reactive({
      shiny::req(r$step_map_auxiliary_fields_accordion_fully_done)

      mermaid_col <- get_config("labelset_id_column")[["mermaid_join"]]

      # Create table of mapping that does exist by left joining annotations' labels to the known mapping
      annotations_labels() %>%
        dplyr::left_join(known_mapping(), by = setNames(mermaid_col, r$provider_col_id)) %>%
        # Put blanks first, then arrange alphabetically
        dplyr::mutate(.is_na = is.na(mermaid_attribute)) %>%
        dplyr::arrange(
          dplyr::desc(.is_na),
          mermaid_attribute
        ) %>%
        dplyr::select(-.is_na, -dplyr::all_of(r$provider_col_id))
    }) %>%
      shiny::bindEvent(r$step_map_auxiliary_fields_accordion_fully_done)

    # Create an editable table to be shown -----
    output$mapping_table <- rhandsontable::renderRHandsontable({
      # List of possible dropdown values for benthic attribute and growth form

      # For benthic attribute, the levels are the known mapping + anything in `benthic_attributes` that isn't in the known mapping
      benthic_attribute_levels <- c(known_mapping()[["mermaid_attribute"]], r$benthic_attributes) %>%
        unique() %>%
        sort()

      # For growth form, it's `r$growth_forms`

      provider_label_display <- get_config("labelset_code_column")[["table_label"]][[r$provider]]
      mermaid_benthic_attribute_display <- get_config("mermaid_attributes_columns")[["mermaid_attribute"]][["table_label"]]
      mermaid_growth_form_display <- get_config("mermaid_attributes_columns")[["mermaid_growth_form"]][["table_label"]]

      provider_mermaid_mapping() %>%
        rhandsontable::rhandsontable(
          rowHeaders = FALSE, # Remove row numbers
          contextMenu = FALSE, # Disable right clicking
          overflow = "visible", # So dropdown can extend out of table
          stretchH = "all",
          colHeaders = c(provider_label_display, mermaid_benthic_attribute_display, mermaid_growth_form_display)
        ) %>%
        # Make the coralnet label read only
        rhandsontable::hot_col(provider_label_display, readOnly = TRUE) %>%
        # Enable column sorting
        rhandsontable::hot_cols(columnSorting = TRUE) %>%
        rhandsontable::hot_col(mermaid_benthic_attribute_display,
          type = "autocomplete",
          source = benthic_attribute_levels,
          strict = TRUE
        ) %>%
        rhandsontable::hot_col(mermaid_growth_form_display,
          type = "autocomplete",
          source = c(NA_character_, r$growth_forms), # To allow it to be empty?
          strict = TRUE
        ) %>%
        # Highlight cells that need to be filled out %>%
        rhandsontable::hot_col(mermaid_benthic_attribute_display, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              // Add down arrow back in, it goes missing for some reason <3
              var arrow = document.createElement('div');
              arrow.classList.add('htAutocompleteArrow');
              arrow.innerHTML = '&#9660';
              td.appendChild(arrow);

             if (value === null) {
              // Use Coral, not dark coral
              td.style.background = '#cf675b';
             }

           td
           }")
    })
    # The flow is:
    # Show the mapping that there is, but make the MERMAID attribute editable if that's not what they want to map it to
    # If there is no mapping, make them select from a MERMAID attribute

    # Should there be an option at this point to save the data with the MERMAID attribute - so if they re-upload later on, they won't have to go through this process again? And it could check for MERMAID specific columns? Is that too complicated?

    # Put the editable table in an accordion ----
    shiny::observe({
      shiny::req(r$step_map_auxiliary_fields_accordion_fully_done)
      shiny::req(provider_mermaid_mapping())

      r$accordion_map_provider_labels <- bslib::accordion_panel(
        title = shiny::h2(get_copy("mapping", "title", r$provider)),
        value = "map-provider-labels",
        shiny::tagList(
          spaced(get_copy("mapping", "text", r$provider)),
          indent(
            shiny::div(
              id = "handsontable-parent",
              rhandsontable::rHandsontableOutput(ns("mapping_table"))
            ),
            shiny::div(
              class = "space",
              shiny::div(id = "confirm-disabled", spaced(get_copy("mapping", "not_allowed_to_confirm", r$provider))),
              shinyjs::disabled(success_button(ns("save_mapping"), "Confirm")),
              shinyjs::hidden(button(ns("edit"), "Edit"))
            )
          )
        )
      )

      r$step_map_provider_labels_accordion_made_done <- TRUE
    })

    edited_provider_mermaid_mapping <- shiny::reactive({
      shiny::req(input$mapping_table)
      rhandsontable::hot_to_r(input$mapping_table)
    })

    # Enable confirming of mapping widget ----
    # Closing disabled unless all of `mermaid_attribute` are not NA
    # If none of `mermaid_attribute` are NA, then enable exiting the widget
    # Flag that the mapping is valid, and save the final mapping
    shiny::observe({
      shiny::req(r$step_map_provider_labels_accordion_made_done)
      # The data in the table is named after the output, so it's input$mapping_table
      # Need to convert it to an R data frame using rhandsontable::hot_to_r()

      no_empty_mapping <- edited_provider_mermaid_mapping() %>%
        dplyr::filter(is.na(mermaid_attribute)) %>%
        nrow() == 0

      all_valid_mapping <- edited_provider_mermaid_mapping() %>%
        dplyr::filter(!is.na(mermaid_attribute)) %>%
        dplyr::as_tibble() %>%
        dplyr::filter(!mermaid_attribute %in% c(
          r$benthic_attributes,
          # TODO -> discrepancy here between known_mapping and r$benthic_attributes
          known_mapping()[["mermaid_attribute"]]
        )) %>%
        nrow() == 0

      mapping_valid <- no_empty_mapping & all_valid_mapping

      if (mapping_valid) {
        shinyjs::hide("confirm-disabled", asis = TRUE)
        shinyjs::enable("save_mapping")
      } else {
        shinyjs::show("confirm-disabled", asis = TRUE)
        shinyjs::disable("save_mapping")
        r$provider_mermaid_mapping <- NULL
      }
    })

    # When the label mapping has been confirmed ----
    shiny::observe({
      r$provider_mermaid_mapping <- edited_provider_mermaid_mapping()
      r$step_map_provider_labels_done <- TRUE
      # Disable confirm, show and enable "edit"
      shinyjs::disable("save_mapping")
      shinyjs::show("edit")
      shinyjs::enable("edit")
      # Disable the table by making fields read only
      disable_mapping_table(ns("mapping_table"))
    }) %>%
      shiny::bindEvent(input$save_mapping)

    # Re-enable table when "edit" is clicked
    shiny::observe({
      shinyjs::disable("edit")
      shinyjs::enable("save_mapping")
      enable_mapping_table(ns("mapping_table"))
      r$step_map_provider_labels_done <- FALSE
    }) %>%
      shiny::bindEvent(input$edit)

    # Create a new version of the annotations with the mapping ----
    shiny::observe({
      shiny::req(r$step_map_provider_labels_done)
      shiny::req(r$provider_mermaid_mapping)

      mermaid_attributes_cols <- get_config("mermaid_attributes_columns") %>%
        purrr::map("column")
      mermaid_attributes_cols <- setNames(names(mermaid_attributes_cols), mermaid_attributes_cols)

      r$annotations_mapped <- r$annotations %>%
        dplyr::left_join(r$provider_mermaid_mapping, r$provider_col_code) %>%
        dplyr::rename(mermaid_attributes_cols)

      r$step_map_provider_joined_done <- TRUE
    })
  })
}

disable_mapping_table <- function(id) {
  shinyjs::runjs(glue::glue("window.HTMLWidgets.findAll('#$id$')[0].hot.updateSettings({readOnly: true, contextMenu: false, disableVisualSelection: true, columnSorting: false})", .open = "$", .close = "$"))

  # Disable pointer events on actual table, add style
  # Not allowed cursor on parent div, add style
  shinyjs::runjs(glue::glue("let tempTable = document.getElementById('$id$'); tempTable.getElementsByClassName('ht_master')[0].style.pointerEvents = 'none'; tempTable.getElementsByClassName('ht_clone_top')[0].style.pointerEvents = 'none'; tempTable.style.cursor = 'not-allowed';", .open = "$", .close = "$"))
}

enable_mapping_table <- function(id) {
  shinyjs::runjs(glue::glue("window.HTMLWidgets.findAll('#$id$')[0].hot.updateSettings({readOnly: false, contextMenu: true, disableVisualSelection: false, columnSorting: true})", .open = "$", .close = "$"))

  # Allow pointer events on actual table, remove style
  # Regular cursor on parent div, remove style
  shinyjs::runjs(glue::glue("let tempTable = document.getElementById('$id$');  tempTable.getElementsByClassName('ht_master')[0].style.pointerEvents = '';  tempTable.getElementsByClassName('ht_clone_top')[0].style.pointerEvents = '';  tempTable.style.cursor = '';", .open = "$", .close = "$"))
}
