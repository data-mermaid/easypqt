#' reshape_annotations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reshape_annotations_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' reshape_annotations Server Functions
#'
#' @noRd
mod_reshape_annotations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reshape annotations into the format required for ingestion ----

    shiny::observe({
      ingestion_data <- r$annotations_mapped

      ## Site: already mapped ----

      ## Management: already mapped ----

      ## Sample date: split date into Year, Month, Day ----
      ingestion_data <- ingestion_data %>%
        dplyr::mutate(
          `Sample date: Year *` = lubridate::year(Date),
          `Sample date: Month *` = lubridate::month(Date),
          `Sample date: Day *` = lubridate::day(Date)
        )

      ## Transect number: already mapped ----

      ## Quadrat: Unique quadrat # by site, date, MR, transect number, image file name ----

      ### Derive "SU" ----

      ingestion_data <- ingestion_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(...su = glue::glue_collapse(c(`Site *`, `Management *`, `Transect number *`), sep = "******")) %>%
        dplyr::ungroup() %>%
        dplyr::group_by()

      ### Identify unique quadrats (images) with an SU ----

      ingestion_data <- ingestion_data %>%
        dplyr::distinct(...su, Name) %>%
        dplyr::group_by(...su) %>%
        dplyr::mutate(`Quadrat *` = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(ingestion_data, by = c("...su", "Name"))

      ## Number of quadrats: determined by count of quadrats in SU
      ingestion_data <- ingestion_data %>%
        dplyr::group_by(...su) %>%
        dplyr::mutate(`Number of quadrats *` = dplyr::n_distinct(`Quadrat *`)) %>%
        dplyr::ungroup()

      ## Number of points per quadrat: count of points in unique quadrat ----
      # TODO - this needs to be the same within an SU, so maybe flag somehow if it is not?
      ingestion_data <- ingestion_data %>%
        dplyr::add_count(...su, name = "total_transect_points") %>%
        dplyr::mutate(`Number of points per quadrat *` = total_transect_points / `Number of quadrats *`)

      ## Number of points: count of points with benthic attribute/growth form in a quadrat
      ingestion_data <- ingestion_data %>%
        dplyr::add_count(...su, `Quadrat *`, `Benthic attribute *`, `Growth form *`, name = "Number of points *")

      # Select only relevant fields
      r$ingestion_data <- ingestion_data %>%
        dplyr::select(dplyr::any_of(names(r$template)))
    }) %>%
      shiny::bindEvent(r$annotations_mapped)

    # Add default fields ----
    shiny::observe(priority = 0, {
      ingestion_data_with_defaults <- r$ingestion_data

      ## Depth: default to 0 ----
      ingestion_data_with_defaults <- ingestion_data_with_defaults %>%
        dplyr::mutate(`Depth *` = 0)

      ## Transect length surveyed: default to 0 -----
      ingestion_data_with_defaults <- ingestion_data_with_defaults %>%
        dplyr::mutate(`Transect length surveyed *` = 0)

      ## Quadrat size: default to 0 ----
      ingestion_data_with_defaults <- ingestion_data_with_defaults %>%
        dplyr::mutate(`Quadrat size *` = 0)

      ## First quadrat number: 1 ----
      ingestion_data_with_defaults <- ingestion_data_with_defaults %>%
        dplyr::mutate(`First quadrat number` = 1)

      ## Observer emails: hit "me" endpoint ----
      me <- mermaidr::mermaid_get_me(token = r$mermaidr_token)
      observer_email <- me[["email"]] %>% unique()
      ingestion_data_with_defaults <- ingestion_data_with_defaults %>%
        dplyr::mutate(`Observer emails *` = observer_email)

      # Get fields in the right order
      ingestion_data_with_defaults <- ingestion_data_with_defaults %>%
        dplyr::select(dplyr::any_of(names(r$template)))

      r$ingestion_data_with_defaults <- ingestion_data_with_defaults
    }) %>%
      shiny::bindEvent(r$ingestion_data)
  })
}

## To be copied in the UI
# mod_reshape_annotations_ui("reshape_annotations_1")

## To be copied in the server
# mod_reshape_annotations_server("reshape_annotations_1")
