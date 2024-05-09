## code to prepare `upload` dataset goes here

library(dplyr)

data <- readr::read_csv("inst/extdata/coralnet-sample-exports/upload/annotations-image_metadata_date_and_auxiliary.csv")

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

# Wrong values ----

wrong_values <- data %>%
  filter_data("wrong_values")

usethis::use_data(wrong_values, overwrite = TRUE)

# Some good, some wrong ----

some_good_some_wrong <-data %>%
  filter_data(c("good", "wrong_values"))

usethis::use_data(some_good_some_wrong, overwrite = TRUE)

