#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  authenticated <- shiny::reactiveVal(FALSE)

  # Set up reactive values ----
  r <- shiny::reactiveValues(
    auxiliary_columns_map = get_config("auxiliary_columns_map"),
    page_length = 10,
    map_annotations_accordion_made = FALSE,
    aux_mapped = FALSE,
    preview_confirm_shown = 0,
    dev = FALSE,
    prod = FALSE,
    # dev_scenario = "empties"
    # dev_scenario = "wrong_values"
    dev_scenario = "good_data"
    # dev_scenario = "some_good_some_wrong"
    # dev_scenario = "transect_decimal"
  )

  # Reset ----
  mod_reset_server("reset")

  # Authenticate ----
  mod_authenticate_server("authenticate", r)

  # Get projects ----
  # This will also get the project template/options, and flag if they are not an admin of the selected project
  mod_select_project_server("select_project", r)

  # Upload CoralNet annotations ----
  # (only once confirmed that they are a project admin)
  mod_upload_annotations_server("upload_annotations", r)

  # Parse annotations -----
  # Map and check auxiliary fields
  # Map CoralNet labels to MERMAID attributes
  mod_parse_annotations_server("parse_annotations", r)

  # Reshape annotations for ingestion ----
  mod_reshape_annotations_server("reshape_annotations", r)

  # Preview/confirm ingestion ----
  mod_ingestion_preview_and_confirm_server("preview_and_confirm", r)

  # Do ingestion ----
  mod_ingestion_do_server("ingest", r)

  # Ingestion results -----

  # Insert and open accordions / close them when that step is done, since they go into global ns ----

  ##  Map annotations ----
  shiny::observe({
    shiny::req(r$accordion_map_annotation_fields)

    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_map_annotation_fields)

    # Open panel
    bslib::accordion_panel_open("accordion", "map-annotation-fields")

    r$map_annotations_accordion_made <- TRUE
  })

  # # Hide annotation page counter if less than 10 rows
  # shiny::observe({
  #   Sys.sleep(5)
  #   shiny::req(r$map_annotations_accordion_made)
  #
  #   if (r$hide_annotation_preview_nav) {
  #     browser()
  #     shinyjs::runjs('document.getElementById("parse_annotations-data_preview").getElementsByClassName("dataTables_paginate")[0].style.display = "none"')
  #   }
  # })

  # Close panel if all annotations are good
  shiny::observe({
    shiny::req(r$all_aux_fields_valid)
    bslib::accordion_panel_close("accordion", "map-annotation-fields")
  }) %>%
    shiny::bindEvent(r$all_aux_fields_valid)

  ## Map labels ----

  shiny::observe({
    shiny::req(r$accordion_map_coralnet_labels)

    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_map_coralnet_labels)

    # Open panel
    bslib::accordion_panel_open("accordion", "map-coralnet-labels")
  })

  # Close panel if all labels are good
  shiny::observe({
    shiny::req(r$coralnet_mapping_valid)
    bslib::accordion_panel_close("accordion", "map-coralnet-labels")
  }) %>%
    shiny::bindEvent(r$coralnet_labels_on_edit)

  ## Preview/download/confirm ---

  shiny::observe({
    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_preview_download_confirm)

    # Open panel
    bslib::accordion_panel_open("accordion", "preview-download-confirm")
  })
}
