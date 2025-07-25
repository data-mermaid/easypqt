## code to prepare `test_reefcloud_data` dataset goes here

library(dplyr)
library(mermaidr)
library(readr)

set.seed(1234)

# Unzip

unzip(here::here("scratch", "sample_data.zip"), exdir = "scratch")

# Read in sample data
sample_file <- here::here("scratch", "Catlin_Seaview_Survey_Soloms-fixed_benthic_ids.csv")

x_raw <- read_csv(sample_file)

# Delete sample
file.remove(sample_file)

# Sample a set of SUs and images
x <- x_raw %>%
  mutate(site_management = coalesce(site_management, "no_management"))

sus <- x %>%
  distinct(site_name, `survey_start_date (UTC)`, site_management, survey_transect_number) %>%
  sample_n(5) %>%
  mutate(su = row_number())

x <- x %>%
  inner_join(sus, by = join_by(site_name, site_management, `survey_start_date (UTC)`, survey_transect_number))

sus_images <- x %>%
  distinct(su, image_name) %>%
  group_by(su) %>%
  sample_n(5) %>%
  ungroup()

x <- x %>%
  inner_join(sus_images, by = c("su", "image_name"))

# Match sites and managements to sample project

p <- mermaid_search_my_projects("Aceh Jaya Coastal Park")

sites <- mermaid_get_project_sites(p)

x_sites <- x %>%
  distinct(site_name)

sample_sites <- sites %>%
  sample_n(nrow(x_sites)) %>%
  select(name)

sample_sites <- bind_cols(x_sites, sample_sites)

managaments <- mermaid_get_project_managements(p)

sample_managements <- managaments %>%
  sample_n(1) %>%
  pull(name)

x <- x %>%
  left_join(sample_sites, by = c("site_name")) %>%
  select(-site_name) %>%
  rename(site_name = name) %>%
  mutate(site_management = sample_managements)

# Select correct names, in order

x <- x %>%
  select(all_of(names(x_raw)))

# Make a couple of labels not already mapped, to test with selecting benthic attribute etc

# TODO

write_csv(x, "inst/extdata/test_reefcloud_aceh_larger.csv")
