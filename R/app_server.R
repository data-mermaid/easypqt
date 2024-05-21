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
    aux_mapped = 0,
    dev = FALSE,
    # dev_scenario = "empties"
    # dev_scenario = "wrong_values"
    dev_scenario = "good_data"
    # dev_scenario = "some_good_some_wrong"
    # dev_scenario = "transect_decimal"
  )

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

  # Preview ingestion ----
  mod_ingestion_preview_server("preview", r)

  # Confirm ingestion ----
  mod_ingestion_confirm_server("confirm", r)

  # Do ingestion ----

  # Ingestion results -----

  # Insert and open accordions / close them when that step is done, since they go into global ns ----

  ##  Map annotations ----
  shiny::observe({
    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_map_annotation_fields)

    # Open panel
    bslib::accordion_panel_open("accordion", "map-annotation-fields")
  })

  # Close panel if all annotations are good
  # TODO - not quite right, need to work on this
  shiny::observe(
    {
      shiny::req(r$all_aux_fields_valid)
      shiny::req(r$aux_fields_on_edit) # Ensures this happens after "confirm" is disabled and "edit" is enabled
      bslib::accordion_panel_close("accordion", "map-annotation-fields")
    }
  ) %>%
    shiny::bindEvent(r$aux_fields_on_edit)
}
