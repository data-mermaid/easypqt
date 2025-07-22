## code to prepare `dev_reefcloud_classification_labelmappings` dataset goes here

# ReefCloud label mappings are not on dev, so get them from prod
# We do not have the IDs from the labelmapping (joining onto benthicattributes reference later on), so just ensure it's benthic attributes that are only _in_ dev

library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)

# Get label mappings

get_and_parse <- function(path) {
  request(path) %>%
    req_perform() %>%
    resp_body_string() %>%
    fromJSON() %>%
    pluck("results") %>%
    as_tibble()
}

labelmappings <- get_and_parse("https://api.datamermaid.org/v1/classification/labelmappings/?limit=5000&provider=ReefCloud")

# Get benthic attributes from dev

benthic_attributes <- get_and_parse("https://dev-api.datamermaid.org/v1/benthicattributes/?limit=5000") %>%
  select(name)

# Only keep benthic attributes that are in dev

labelmappings <- labelmappings %>%
  inner_join(benthic_attributes, by = c("benthic_attribute_name" = "name"))

# Rename columns and dataset, select cols

dev_reefcloud_classification_labelmappings <- labelmappings %>%
  rename(benthic_attribute = benthic_attribute_name, growth_form = growth_form_name) %>%
    select(-benthic_attribute_id, -growth_form_id)

usethis::use_data(dev_reefcloud_classification_labelmappings, overwrite = TRUE)
