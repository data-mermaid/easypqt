# Make CoralNet/MERMAID attribute mapping an object available in app

# From provider_ImageAnalysis google drive folder
# File: CoralNetMermaidMatchedCoralFocusModel2Reassign_master

library(readr)
library(dplyr)

provider_mermaid_attributes <- read_csv(here::here("data-raw", "CoralNetMermaidMatchedCoralFocusModel2Reassign_master.csv")) %>%
  filter(`Public sources using` >= 5) %>%
  select(provider_label = `Default short code`, mermaid_attribute = MERMAID_BA, mermaid_growth_form = MERMAID_GF) %>%
  arrange(provider_label)

usethis::use_data(provider_mermaid_attributes, overwrite = TRUE)
