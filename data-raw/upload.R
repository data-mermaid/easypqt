## code to prepare `upload` dataset goes here

upload <- readr::read_csv("inst/extdata/coralnet-sample-exports/upload/annotations-image_metadata_date_and_auxiliary.csv")

usethis::use_data(upload, overwrite = TRUE)
