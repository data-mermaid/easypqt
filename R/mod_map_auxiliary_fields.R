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
  # No UI for this, since an accordion is created/opened in server
  tagList()
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
        r$columns_map,
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
      names(get_config("coralnet_columns_map")),
      \(x)
      shiny::observe({
        r$columns_map[[x]]["value"] <- list(input[[x]])
      }) %>%
        shiny::bindEvent(input[[x]])
    )

    ## Go through each and disable other columns' aux fields ----
    shiny::observe({
      shiny::req(r$step_map_auxiliary_fields_accordion_made_done)

      # Go through each, and disable the other selected options
      purrr::walk(
        names(r$columns_map),
        \(x) {
          disable_options <- r$columns_map[names(r$columns_map) != x] %>%
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

      cols_mapped <- r$columns_map %>%
        purrr::map("value") %>%
        purrr::compact()

      r$step_fields_setup_done <- length(cols_mapped) == length(r$columns_map)
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
      mapped_cols_names <- r$columns_map %>%
        purrr::map("column")
      mapped_cols_aux <- r$columns_map %>%
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

    ## Restart if needed ----
    shiny::observe({
      shiny::removeModal()
      mod_reset_server("reset", r, show_ui = FALSE, show_confirm = FALSE)
    }) %>%
      shiny::bindEvent(input$incorrect_reset)
  })
}

## To be copied in the UI
# mod_map_auxiliary_fields_ui("map_auxliary_fields")

## To be copied in the server
# mod_map_auxiliary_fields_server("map_auxliary_fields")

make_mapping_dropdown_ui <- function(auxiliary_column_map, auxiliary_column, r, ns) {
  selected <- null_if_dev(r$dev, glue::glue("Aux{number}", number = which(names(shiny::isolate(r$columns_map)) == auxiliary_column)))

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
