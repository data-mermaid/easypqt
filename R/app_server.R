# Taken from auth0 package, but skips call to "userinfo" endpoint, which is unauthorized, and works better with mermaidr tokening system to recognize that the token is being called from Shiny and is therefore fresh and does not need to be refreshed in browser again

auth0_server_verify <- function(session, app, api, state) {
  u_search <- session[["clientData"]]$url_search
  params <- shiny::parseQueryString(u_search)

  if (auth0:::has_auth_code(params, state)) {
    cred <- httr::oauth2.0_access_token(api, app(redirect_uri), params$code)
    mermaidr_token <- mermaidr:::mermaid2.0_token(
      app = app(redirect_uri), endpoint = api, cache = FALSE, credentials = cred,
      user_params = list(grant_type = "authorization_code")
    )
    # Add $shiny = TRUE to the credentials
    mermaidr_token$credentials$shiny <- TRUE

    mermaidr_token <- httr::config(token = mermaidr_token)

    assign("auth0_credentials", mermaidr_token, envir = session$userData)
  }
}

auth0_server <- function(server, info) {
  disable <- getOption("auth0_disable")
  if (!is.null(disable) && disable) {
    server
  } else {
    if (missing(info)) info <- auth0::auth0_info()
    function(input, output, session) {
      shiny::isolate(auth0_server_verify(session, info$app, info$api, info$state))
      shiny::observeEvent(input[["._auth0logout_"]], logout())
      server(input, output, session)
    }
  }
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- auth0_server(function(input, output, session) {
  authenticated <- shiny::reactiveVal(FALSE)

  # Set up reactive values ----
  r <- shiny::reactiveValues(
    is_project_admin = FALSE,
    page_length = 10,
    step_upload_valid_data_done = FALSE,
    map_annotations_accordion_made = FALSE,
    aux_mapped = FALSE,
    preview_confirm_shown = 0,
    dev = FALSE,
    prod = FALSE,
    reset = NULL,
    upload_contains_required_cols = FALSE,
    step_select_valid_project_done = FALSE,
    step_select_human_or_machine_annotated = FALSE,
    step_upload_valid_data_done = FALSE,
    step_map_auxiliary_fields_accordion_made_done = FALSE,
    step_map_auxiliary_fields_accordion_fully_done = FALSE,
    step_map_provider_labels_accordion_made_done = FALSE,
    step_map_provider_labels_done = FALSE,
    step_map_provider_labels_fully_done = FALSE,
    preview_confirm_shown = 0,
    reset_confirm_counter = 0
  )

  # Get login info and hit initial endpoints ----
  shiny::observe(priority = 9999, {
    shinyjs::runjs("window.history.pushState({}, document.title, window.location.pathname);") # Remove code etc from URL so it can restart/refresh cleanly

    r$mermaidr_token <- session$userData$auth0_credentials

    shiny::req(r$mermaidr_token)

    # - User projects
    r$projects <- mermaidr::mermaid_get_my_projects(token = r$mermaidr_token) %>%
      dplyr::arrange(name)

    # - "me" endpoint
    r$me <- mermaidr::mermaid_get_me(token = r$mermaidr_token)

    # - benthic attributes
    r$benthic_attributes <- mermaidr::mermaid_get_reference("benthicattributes") %>%
      dplyr::filter(status == "Open") %>%
      dplyr::pull(name)

    # - growth forms
    growth_forms <- mermaidr::mermaid_get_endpoint("choices") %>%
      dplyr::filter(name == "growthforms") %>%
      dplyr::pull(data)

    r$growth_forms <- growth_forms[[1]][["name"]] %>%
      sort()

    waiter::waiter_hide()
  })

  # Provider selection ----
  mod_select_provider_server("provider", r)

  # Upload instructions ----
  mod_upload_instructions_server("instructions", r)

  # Reset ----
  mod_reset_server("reset", r)

  # Get projects ----
  # This will also get the project template/options, and flag if they are not an admin of the selected project
  mod_select_project_server("select_project", r)

  # Human or machine annotatable labels ----
  # (only once confirmed that they are a project admin)
  mod_select_human_or_machine_annotated_server("human_or_machine", r)

  # Upload annotations ----
  # (only once confirmed that they are a project admin)
  mod_upload_data_server("upload_data", r)

  # Parse annotations -----
  # If necessary (for CoralNet), map auxiliary fields
  # Check fields
  # Map provider labels to MERMAID attributes
  # Map and check auxiliary fields
  mod_parse_annotations_server("parse_annotations", r)

  # Map labels to MERMAID attributes -----
  mod_map_provider_labels_to_mermaid_server("map_labels", r)

  # Reshape annotations for ingestion ----
  mod_reshape_annotations_server("reshape_annotations", r)

  # Preview/confirm ingestion ----
  mod_ingestion_preview_and_confirm_server("preview_and_confirm", r)

  # Do ingestion ----
  mod_ingestion_do_server("ingest", r)

  # Ingestion results -----

  # Insert and open accordions / close them when that step is done, since they go into global ns ----

  ## Map auxiliary fields ----
  shiny::observe({
    shiny::req(r$step_map_auxiliary_fields_accordion_made_done)

    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_map_annotation_fields)

    # Open panel
    bslib::accordion_panel_open("accordion", "map-auxiliary-fields")

    scroll_to_section("map-auxiliary-fields", accordion = TRUE)
  }) %>%
    shiny::bindEvent(r$step_map_auxiliary_fields_accordion_made_done)

  ### Close panel if all annotations are good ----
  shiny::observe({
    shiny::req(r$step_map_auxiliary_fields_valid_done)
    bslib::accordion_panel_close("accordion", "map-auxiliary-fields")

    r$step_map_auxiliary_fields_accordion_fully_done <- TRUE
  }) %>%
    shiny::bindEvent(r$step_map_auxiliary_fields_valid_done)

  ## Map labels ----

  shiny::observe({
    shiny::req(r$step_map_auxiliary_fields_accordion_fully_done)
    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_map_provider_labels)

    # Open panel
    bslib::accordion_panel_open("accordion", "map-provider-labels")

    # Add JS to check for labels table existing, then fix its height
    shiny::insertUI("head", where = "beforeEnd", shiny::includeScript(app_sys("adjustMappingTableHeight.js")))

    scroll_to_section("map-provider-labels", accordion = TRUE)
  }) %>%
    shiny::bindEvent(r$step_map_auxiliary_fields_accordion_fully_done)

  ### Close panel if all labels are good ----
  shiny::observe({
    shiny::req(r$step_map_provider_labels_done)
    bslib::accordion_panel_close("accordion", "map-provider-labels")

    r$step_map_provider_labels_fully_done <- TRUE
  }) %>%
    shiny::bindEvent(r$step_map_provider_labels_done)

  ## Preview/download/confirm ---

  shiny::observe({
    shiny::req(r$preview_confirm_shown > 0)
    # Insert panel
    bslib::accordion_panel_insert("accordion", r$accordion_preview_download_confirm)

    # Open panel
    bslib::accordion_panel_open("accordion", "preview-download-confirm")

    scroll_to_section("preview-download-confirm", accordion = TRUE)
  }) %>%
    shiny::bindEvent(r$preview_confirm_shown)

  ## Remove panels on reset ----

  shiny::observe({
    shiny::req(r$reset > 0)

    # Works even if the panels have not been created/added <3
    bslib::accordion_panel_remove("accordion", "map-auxiliary-fields")
    bslib::accordion_panel_remove("accordion", "map-provider-labels")
    bslib::accordion_panel_remove("accordion", "preview-download-confirm")
  }) %>%
    shiny::bindEvent(r$reset)
})
