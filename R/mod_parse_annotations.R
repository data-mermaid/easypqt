#' parse_annotations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_annotations_ui <- function(id) {
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

#' parse_annotations Server Functions
#'
#' @noRd
mod_parse_annotations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Map auxiliary fields ----

    # Once the annotations file has been verified to contain the correct fields, need to determine which of the auxiliary fields contain Site, Management, and Transect Number

    # Use a dropdown for each rather than radio buttons - just a bit more complicated to disable etc and to work within a data table

    # It would be nice to allow them to optionally preview the data in here to remind themselves which of the auxiliary columns is which?

    # Generate dropdown UI ----
    # (Shown once annotations are uploaded)
    shiny::observe({
      shiny::req(r$annotations_raw)

      aux_fields_data <- r$annotations_raw %>%
        dplyr::select(dplyr::all_of(r$auxiliary_columns)) %>%
        dplyr::distinct()

      r$hide_annotation_preview_nav <- nrow(aux_fields_data) < r$page_length

      output$data_preview <- aux_fields_data %>%
        DT::datatable(
          rownames = FALSE,
          options = list(dom = "tp", pageLength = r$page_length),
          selection = "none"
        ) %>%
        DT::renderDataTable()

      inputs <- purrr::imap(
        # Just do this once, do not listen to changes in the mapping
        shiny::isolate(r$auxiliary_columns_map),
        \(x, y) {
          make_mapping_dropdown_ui(x, y, r, ns)
        }
      )

      r$accordion_map_annotation_fields <- bslib::accordion_panel(
        title = shiny::h2("Map CoralNet annotation fields"),
        value = "map-annotation-fields",
        indent(
          shiny::h3("Auxiliary fields preview"), # TODO config
          DT::dataTableOutput(ns("data_preview")),
          shiny::h3("Map auxiliary fields"), # TODO config
          inputs
        )
      )

      r$accordion_map_annotation_made <- TRUE
    })

    # Observe each dropdown, and disable an Aux field in other dropdowns if it's already selected - because an auxiliary field cannot map to more than one of Site, Management, or Transect Number

    # Update list of mapped columns ----
    purrr::walk(
      names(get_config("auxiliary_columns_map")),
      \(x)
      shiny::observe(priority = 1, {
        shiny::req(r$annotations_raw)
        if (r$dev) {
          shiny::req(input$site)
        }
        r$auxiliary_columns_map[[x]]["value"] <- list(input[[x]])
        r$aux_mapping_ui_created <- TRUE
      })
    )

    # Go through each and disable other columns' aux fields ----
    shiny::observe({
      shiny::req(r$annotations_raw)
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

    # Once all auxiliary mapping fields have been filled out, flag them for checking non-empty/valid ----
    shiny::observe({
      shiny::req(r$annotations_raw)

      cols_mapped <- r$auxiliary_columns_map %>%
        purrr::map("value") %>%
        purrr::compact()

      r$aux_mapped <- length(cols_mapped) == length(r$auxiliary_columns_map)
    })

    # Check that fields are not empty ----
    # Check that Date, Site, Management, and Transect Number are not empty
    shiny::observe(priority = 1, {
      shiny::req(r$aux_mapped)

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

        show_modal(empty_fields_text)
      } else {
        r$no_empty_fields <- TRUE
      }
    })

    # Check valid values of auxiliary fields ----
    # Validate fields ----
    shiny::observe({
      shiny::req(r$no_empty_fields)

      # Check that sites are ones already entered in the project ----
      site_ui <- check_valid_values(r, "site")

      # Check that managements are ones already entered in the project ----
      management_ui <- check_valid_values(r, "management")

      # Check that transect number is an integer ----
      transect_number_ui <- check_integer_values(r, "transect_number")

      if (site_ui[["valid"]] & management_ui[["valid"]] & transect_number_ui[["valid"]]) {
        r$all_aux_fields_valid <- TRUE
      } else {
        r$all_aux_fields_valid <- FALSE

        show_modal(
          title = "Validating fields",
          site_ui[["ui"]],
          management_ui[["ui"]],
          transect_number_ui[["ui"]],
          shiny::br(),
          shiny::div("Please fix invalid values in CoralNet before continuing.")
        )
      }
    }) %>%
      shiny::bindEvent(r$no_empty_fields)

    # Once all auxiliary mapping have been checked, disable the inputs - cannot edit them anymore
    shiny::observe({
      # IF they are all good, then:
      shiny::req(r$all_aux_fields_valid)
      shiny::req(r$no_empty_fields)
      r$aux_fields_on_edit <- TRUE
      # Disable all inputs
      disable_picker_input(ns("site"))
      disable_picker_input(ns("management"))
      disable_picker_input(ns("transect_number"))
    }) %>%
      shiny::bindEvent(r$all_aux_fields_valid)

    # Rename columns in data according to auxiliary fields mapping ----
    shiny::observe({
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
      shiny::bindEvent(r$all_aux_fields_valid)

    # Show date/site/management, confirm and continue ----
    # TODO

    # Map CoralNet labelsets to MERMAID ----
    mod_map_coralnet_labels_to_mermaid_server("map_coralnet_labels", r)
  })
}

## To be copied in the UI
# mod_parse_annotations_ui("parse_annotations_1")

## To be copied in the server
# mod_parse_annotations_server("parse_annotations_1")


make_mapping_dropdown_ui <- function(auxiliary_column_map, auxiliary_column, r, ns) {
  selected <- null_if_dev(r$dev, glue::glue("Aux{number}", number = which(names(shiny::isolate(r$auxiliary_columns_map)) == auxiliary_column)))

  shiny::fluidRow(
    shiny::column(
      width = 6,
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
          noneSelectedText = "Select an auxiliary field"
        )
      )
    )
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
    res <- glue::glue("{lookup} values good")
    # TODO
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(r$auxiliary_columns_map[[lookup]][["label"]]),
        res
      )
  )
}

check_integer_values <- function(r, lookup) {
  values <- r$annotations_raw[r$auxiliary_columns_map[[lookup]][["value"]]]
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
    res <- glue::glue("{lookup} numbers good")
    # TODO
  }

  list(
    valid = all_valid,
    ui =
      shiny::tagList(
        shiny::h3(r$auxiliary_columns_map[[lookup]][["label"]]),
        res
      )
  )
}
