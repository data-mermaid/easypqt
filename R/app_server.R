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

  # Reset ----
  mod_reset_server("reset")

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
})
