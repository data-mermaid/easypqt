# Read and understand CoralNet structure of additional fields, what we need for mapping and transforming

# From project: https://coralnet.ucsd.edu/source/4254/

library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(mermaidr)
library(lubridate)

files <- fs::dir_ls(here::here("inst", "extdata", "coralnet-sample-exports"), glob = "*.csv")

types <- basename(files) %>%
  str_remove("\\.csv") %>%
  str_remove("annotations-")

files <- setNames(files, types)

annotations <- map(files, read_csv)

annotation_optional_field_names <- map(annotations, names)

# No optional:
# Name, Row, Column, Label

# Annotator info:
# Annotator, Date annotated

# Machine suggestions:
# Machine suggestion 1, Machine confidence 1, ... , Machine suggestion 5, Machine confidence 5

# Image metadata - date and auxiliary fields
# Date, Aux 1, ... , Aux 5

# Image metadata - other fields
# Height (cm), Latitude, Longitude, Depth, Camera, Photographer, Water quality, Strobes, Framing gear used, White balance card, Comments

# All optional fields
# Just checking that it contains exactly "No optional" + all others
all_names <- annotation_optional_field_names[c(
  "annotator_info", "image_metadata_date_and_auxiliary",
  "image_metadata_other", "machine_suggestions", "no_optional"
)] %>%
  unlist(use.names = FALSE) %>%
  unique() %>%
  sort()

identical(annotation_optional_field_names[["all_optional"]] %>% sort(), all_names)

# We only need the date and auxiliary fields, but may be good to check if other fields are present, and ONLY the other known fields
# OR, we can tell them to only export those fields - that might be easier
# Otherwise they may mistakenly have a field that they think will get used?

aux_annotations <- annotations[["image_metadata_date_and_auxiliary"]]

# Match labels to MERMAID mappings

aux_annotations %>%
  distinct(Label) %>%
  left_join(mermaidrcoralnet::coralnet_mermaid_attributes, by = c("Label" = "coralnet_label"))

# This one has none.... but pretend there were some?

# Reshape the data

# Understand shape that the data needs to take:

template <- mermaid_get_my_projects() %>%
  slice(2) %>%
  mermaid_import_get_template_and_options("benthicpqt")

template <- template$Template

names(template)

# What we need from aux fields:
# Site
# Management
# Date (can we use the Date field? or do we need a different aux field?)
# Transect number

# What we'll auto-create:
# Quadrat number -> just number from 1 onward
# Quadrat number start -> 1
# Number of quadrats -> Number of images in site/date/transect number
# Number of points per quadrat? -> is this just the number of points per quadrat in the data? Or is it possible that nothing is detected, so there is no point?

# Is it possible for them to upload multiples of the same image name onto CoralNet?
# We'll need to ID unique quadrats by Name, Date, Site, Transect Number then

# The order of Aux fields will have to be the same, e.g. we have to dictate that:
# Aux1 is Site
# Aux2 is Management
# Aux3 is Transect number
# (if Date can be used as sample date - otherwise, use Aux1 for Date and shift all the rest)

# For now, just treat Name (image) as unique

aux_annotations_renamed <- aux_annotations %>%
  rename(
    `Site *` = Aux1, `Management *` = Aux2, `Transect number *` = Aux3,
    `Benthic attribute *` = Label
  ) # This one is bad because it doesn't actually map yet - but just to get an idea of the form

quadrat_numbers <- aux_annotations_renamed %>%
  distinct(Name) %>%
  mutate(`Quadrat *` = row_number())

number_of_quadrats <- nrow(quadrat_numbers)

quadrat_number_start <- 1

aux_annotations_summarized <- aux_annotations_renamed %>%
  left_join(quadrat_numbers, by = "Name") %>%
  group_by(`Site *`, `Management *`, `Transect number *`, Date, `Quadrat *`) %>%
  mutate(`Number of points per quadrat *` = n()) %>%
  ungroup() %>%
  count(`Site *`, `Management *`, `Transect number *`, Date, `Quadrat *`, `Benthic attribute *`, `Number of points per quadrat *`, name = "Number of points *") %>%
  mutate(`Number of quadrats *` = number_of_quadrats,
         `First quadrat number` = 1)

# Derive date fields
# Just for now, put in a fake date, since there aren't any
# Question - what format does this have to be in CoralNet? Free, or calendar picker?
aux_annotations_summarized <- aux_annotations_summarized %>%
  mutate(Date = as.Date("2024-01-01"))

aux_annotations_summarized <- aux_annotations_summarized %>% mutate(
  `Sample date: Year *` = year(Date),
  `Sample date: Month *` = month(Date),
  `Sample date: Day *` = day(Date)
)

# What's left?

setdiff(names(template), names(aux_annotations_summarized))

#  [1] "Sample time"                "Depth *"
#  [3] "Transect label"             "Transect length surveyed *"
#  [5] "Quadrat size *"             "Reef slope"
#  [7] "Visibility"                 "Current"
#  [9] "Relative depth"             "Tide"
#  [11] "Sample unit notes"         "Observer emails *"
#  [13] "Growth form"

# Only depth, transect length surveyed, quadrat size, and observer emails are mandatory fields
# And they can input those in MERMAID
# But the API has to allow for those fields to be empty

# Need to create a data frame with all of the fields, even if they are empty

# So use the template to do this

empty_template <- template %>%
  slice(0)

annotations_with_all_fields <- aux_annotations_summarized %>%
  # NOTE - the template comes through with all of these as character, but does the API care how they are ingested? This might cause an issue
  mutate_all(as.character) %>%
  bind_rows(empty_template) %>%
  select(all_of(names(empty_template)))
