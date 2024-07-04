## code to prepare `upload` dataset goes here

library(dplyr)
library(tidyr)

data <- readr::read_csv("data-raw/test_data.csv")

filter_data <- function(data, scenario) {
  data %>%
    filter(Scenario %in% scenario) %>%
    select(-Scenario)
}

# Empties ----

empties <- data %>%
  filter_data("empties")

usethis::use_data(empties, overwrite = TRUE)

# Good data ----

good_data <- data %>%
  filter_data("good")

usethis::use_data(good_data, overwrite = TRUE)

# Good data, large dataset -----

set.seed(1234)

good_data_large <- good_data %>%
  select(-Row, -Column, -Label) %>%
  distinct() %>%
  mutate(join = TRUE) %>%
  left_join(
    crossing(Row = 1:500, Column = 1:100) %>%
      mutate(join = TRUE),
    by = "join",
    relationship = "many-to-many"
  )

coralnet_labels <- coralnet_mermaid_attributes %>%
  select(Label = coralnet_label) %>%
  sample_n(nrow(good_data_large), replace = TRUE)

good_data_large <- good_data_large %>%
  bind_cols(coralnet_labels) %>%
  select(-join)

readr::write_csv(good_data_large, here::here("test_large.csv"))

# Wrong values ----

wrong_values <- data %>%
  filter_data("wrong_values")

usethis::use_data(wrong_values, overwrite = TRUE)

# Some good, some wrong ----

some_good_some_wrong <- data %>%
  filter_data(c("good", "wrong_values"))

usethis::use_data(some_good_some_wrong, overwrite = TRUE)

# Decimal transect ----

transect_decimal <- data %>%
  filter_data("transect_decimal")

usethis::use_data(transect_decimal, overwrite = TRUE)
