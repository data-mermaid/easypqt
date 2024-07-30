#' map_auxliary_fields UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_auxiliary_fields_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::div(),
    # Parse CoralNet annotations auxiliary fields ----
    # Show date/site/management, confirm and continue ----
    # TODO
    # Check valid values of fields ----
    # Map labelsets ----
    mod_map_coralnet_labels_to_mermaid_ui(ns("map_coralnet_labels"))
  )
}

#' map_auxliary_fields Server Functions
#'
#' @noRd
mod_map_auxiliary_fields_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Once the annotations file has been verified to contain the correct fields, need to determine which of the auxiliary fields contain Site, Management, and Transect Number

    # Use a dropdown for each rather than radio buttons - just a bit more complicated to disable etc and to work within a data table

    # Generate dropdown UI ----
    # (Shown once annotations are uploaded)
    shiny::observe({
      shiny::req(r$step_upload_valid_data_done)

      aux_fields_data <- r$annotations_raw %>%
        dplyr::select(dplyr::all_of(r$auxiliary_columns)) %>%
        dplyr::distinct()

      output$data_preview <- aux_fields_data %>%
        DT::datatable(
          rownames = FALSE,
          options = list(dom = "tp", pageLength = r$page_length),
          selection = "none",
          callback = DT::JS("$.fn.dataTable.ext.errMode = 'none';") # To eliminate JS popups with mismatch of cols / accordion / table updating, it's just annoying
        ) %>%
        DT::renderDataTable()

      output$inputs <- purrr::imap(
        r$auxiliary_columns_map,
        \(x, y) {
          make_mapping_dropdown_ui(x, y, r, ns)
        }
      ) %>%
        shiny::renderUI()

      r$accordion_map_annotation_fields <- bslib::accordion_panel(
        title = shiny::h2(get_copy("auxiliary", "title")),
        value = "map-auxiliary-fields",
        shiny::tagList(
          spaced(get_copy("auxiliary", "text")),
          shiny::hr(),
          indent(
            shiny::h3(get_copy("auxiliary", "preview")),
            spaced(get_copy("auxiliary", "preview_text")),
            DT::dataTableOutput(ns("data_preview")),
            shiny::hr(),
            shiny::h3(get_copy("auxiliary", "map")),
            spaced(get_copy("auxiliary", "map_text")),
            shiny::uiOutput(ns("inputs"))
          )
        )
      )

      r$step_map_auxiliary_fields_accordion_made_done <- TRUE
    }) %>%
      shiny::bindEvent(r$step_upload_valid_data_done)

    # Observe each dropdown, and disable an Aux field in other dropdowns if it's already selected ----
    # because an auxiliary field cannot map to more than one of Site, Management, or Transect Number

    ## Update list of mapped columns ----
    purrr::walk(
      names(get_config("auxiliary_columns_map")),
      \(x)
      shiny::observe({
        r$auxiliary_columns_map[[x]]["value"] <- list(input[[x]])
      }) %>%
        shiny::bindEvent(input[[x]])
    )

    ## Go through each and disable other columns' aux fields ----
    shiny::observe({
      shiny::req(r$step_map_auxiliary_fields_accordion_made_done)

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

    ## Once all auxiliary mapping fields have been filled out, flag them for checking non-empty/valid ----
    shiny::observe({
      shiny::req(r$step_map_auxiliary_fields_accordion_made_done)

      cols_mapped <- r$auxiliary_columns_map %>%
        purrr::map("value") %>%
        purrr::compact()

      r$step_map_auxiliary_fields_done <- length(cols_mapped) == length(r$auxiliary_columns_map)

      # Check that fields are not empty ----
      # Check that Date, Site, Management, and Transect Number are not empty

      shiny::req(r$step_map_auxiliary_fields_done)

      show_modal(
        title = get_copy("auxiliary_validating", "title"),
        get_copy("auxiliary_validating", "checking"),
        shiny::br(), # TODO
        footer = NULL
      )
      Sys.sleep(1)

      # Iterate through and check if any values are empty
      check_fields <- append(get_config("additional_columns_map"), r$auxiliary_columns_map)

      empty_fields <- purrr::map(
        check_fields,
        \(x) {
          # Get values for field
          values <- r$annotations_raw[x$value]
          names(values) <- "field"

          # Check if any are NA
          na_values <- values %>% dplyr::filter(is.na(field))
          any_na <- nrow(na_values) > 0

          # Return label of field if any are NA
          if (any_na) {
            x$label
          } else {
            # Otherwise, return NULL
            NULL
          }
        }
      ) %>%
        purrr::compact()

      if (length(empty_fields) > 0) {
        r$no_empty_fields <- FALSE
        empty_fields_skeleton <- get_copy("non_empty_fields")
        empty_fields_list <- make_formatted_list(empty_fields)

        all_fields <- check_fields %>%
          purrr::map("label") %>%
          glue::glue_collapse(sep = ", ", last = ", and ")

        empty_fields_glue <- list(fields = all_fields, list = empty_fields_list)

        empty_fields_text <- skeleton_to_text(empty_fields_skeleton, empty_fields_glue)

        shiny::removeModal()
        show_modal(
          title = get_copy("auxiliary_validating", "title"), empty_fields_text
        )
      } else {
        r$no_empty_fields <- TRUE
      }

      # Check valid values of auxiliary fields ----
      # Validate fields ----
      shiny::req(r$no_empty_fields)

      # Get project template/options ----
      # Show modal that we are getting this?
      template_and_options <- mermaidr::mermaid_import_get_template_and_options(r$project, "benthicpqt", token = r$mermaidr_token)
      r$template <- template_and_options$Template
      r$template_choices <- template_and_options[names(template_and_options) != "Template"] %>%
        purrr::map("choices") %>%
        purrr::compact()

      # Check that sites are ones already entered in the project ----
      site_ui <- check_valid_values(r, "site")

      # Check that managements are ones already entered in the project ----
      management_ui <- check_valid_values(r, "management")

      # Check that transect number is an integer ----
      transect_number_ui <- check_integer_values(r, "transect_number")

      # Hide checking modal
      shiny::removeModal()

      if (site_ui[["valid"]] & management_ui[["valid"]] & transect_number_ui[["valid"]]) {
        r$step_map_auxiliary_fields_valid_done <- TRUE

        # Show modal that all is good
        # Show checking modal
        show_modal(
          title = get_copy("auxiliary_validating", "title"),
          get_copy("auxiliary_validating", "all_valid")
        )
      } else {
        show_modal(
          title = get_copy("auxiliary_validating", "title"),
          shiny::div(class = "validating-aux", get_copy("auxiliary_validating", "fix")),
          site_ui[["ui"]],
          management_ui[["ui"]],
          transect_number_ui[["ui"]]
        )
      }
    })

    shiny::observe({
      # IF they are all good, then:
      shiny::req(r$step_map_auxiliary_fields_valid_done)

      # Once all auxiliary mapping have been checked, disable the inputs - cannot edit them anymore
      # Disable all inputs
      disable_picker_input(ns("site"))
      disable_picker_input(ns("management"))
      disable_picker_input(ns("transect_number"))

      # Rename columns in data according to auxiliary fields mapping ----
      mapped_cols_names <- r$auxiliary_columns_map %>%
        purrr::map("column")
      mapped_cols_aux <- r$auxiliary_columns_map %>%
        purrr::map("value")
      mapped_cols <- setNames(mapped_cols_aux, mapped_cols_names) %>%
        unlist()

      r$annotations <- r$annotations_raw %>%
        dplyr::rename(mapped_cols)

      # Remove auxiliary fields that were not mapped
      extra_aux_fields <- setdiff(r$auxiliary_columns, mapped_cols_aux)

      r$annotations <- r$annotations %>%
        dplyr::select(-dplyr::all_of(extra_aux_fields))
    }) %>%
      shiny::bindEvent(r$step_map_auxiliary_fields_valid_done)

    # Show date/site/management, confirm and continue ----
    # TODO

    # Map CoralNet labelsets to MERMAID ----
    mod_map_coralnet_labels_to_mermaid_server("map_coralnet_labels", r)
  })
}

## To be copied in the UI
# mod_map_auxiliary_fields_ui("map_auxliary_fields")

## To be copied in the server
# mod_map_auxiliary_fields_server("map_auxliary_fields")


make_mapping_dropdown_ui <- function(auxiliary_column_map, auxiliary_column, r, ns) {
  selected <- null_if_dev(r$dev, glue::glue("Aux{number}", number = which(names(shiny::isolate(r$auxiliary_columns_map)) == auxiliary_column)))

  shiny::fluidRow(
    shiny::column(
      width = 3,
      # TODO: Vertically align with input
      shiny::tags$b(auxiliary_column_map[["label"]])
    ),
    shiny::column(
      width = 6,
      shinyWidgets::pickerInput(
        inputId = ns(auxiliary_column),
        label = NULL,
        choices = r$auxiliary_columns,
        selected = selected,
        multiple = TRUE,
        # TODO, CSS styling for this to look like single selection, e.g. darker highlighting and not a check mark
        options = shinyWidgets::pickerOptions(
          maxOptions = 1,
          noneSelectedText = get_copy("auxiliary", "placeholder")
        )
      )
    ) %>% tagAppendAttributes(class = "constrained-col")
  )
}

check_valid_values <- function(r, lookup) {
  aux_lookup <- r$auxiliary_columns_map[[lookup]]
  template_lookup <- aux_lookup[["column"]]
  actual_lookup <- aux_lookup[["value"]]

  valid_values <- r$template_choices[[template_lookup]][["value"]]
  actual_values <- unique(r$annotations_raw[[actual_lookup]])

  invalid_values <- setdiff(actual_values, valid_values)

  all_valid <- length(invalid_values) == 0

  column <- r$auxiliary_columns_map[[lookup]][["label"]]

  if (!all_valid) {
    invalid_values_skeleton <- get_copy("invalid_values")
    invalid_values <- make_formatted_list(invalid_values)
    valid_values <- make_formatted_list(valid_values)

    invalid_values_envir <- list(
      label = r$auxiliary_columns_map[[lookup]][["label"]],
      invalid_values = invalid_values,
      valid_values = valid_values
    )

    res <- skeleton_to_text(invalid_values_skeleton, invalid_values_envir)
  } else {
    res <- glue::glue(get_copy("auxiliary_validating", "column_valid"))
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(column),
        shiny::div(class = "validating-aux", res)
      )
  )
}

check_integer_values <- function(r, lookup) {
  values <- r$annotations_raw[r$auxiliary_columns_map[[lookup]][["value"]]]
  column <- r$auxiliary_columns_map[[lookup]][["label"]]
  names(values) <- "value"

  values_with_numeric <- values %>%
    dplyr::mutate(numeric_value = as.integer(value) %>% suppressWarnings())

  invalid_values <- values_with_numeric %>%
    dplyr::filter(is.na(numeric_value) |
      as.character(numeric_value) != value) # Handles decimals

  all_valid <- nrow(invalid_values) == 0

  if (!all_valid) {
    # Show the invalid ones only

    transect_number_invalid_skeleton <- get_copy("transect_number_not_integer")
    invalid_values <- make_formatted_list(invalid_values[["value"]])

    res <- skeleton_to_text(transect_number_invalid_skeleton, list(invalid_values = invalid_values))
  } else {
    res <- glue::glue(get_copy("auxiliary_validating", "column_valid"))
    # TODO
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(r$auxiliary_columns_map[[lookup]][["label"]]),
        shiny::div(class = "validating-aux", res)
      )
  )
}
