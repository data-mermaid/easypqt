# Make CoralNet/MERMAID attribute mapping an object available in app

library(readr)
library(dplyr)

coralnet_mermaid_attributes <- read_csv2(here::here("data-raw", "CoralNet-MERMAID attributes.csv"))

names(coralnet_mermaid_attributes) <- c("coralnet_label", "mermaid_attribute", "mermaid_growth_form")

usethis::use_data(coralnet_mermaid_attributes, overwrite = TRUE)
