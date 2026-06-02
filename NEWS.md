# easypqt 0.4.0

* Improve code around reactive values, streamlines resetting flow and reduces bugs.
* Validate that number of points per quadrat is consistent for ReefCloud data.
* Fix bug with mapping ReefCloud labels to MERMAID labels when human code is present, but human ID is not.
* Fix bug with enabling "Confirm" button in label mapping, add slight delay to account for stricter rendering in Safari.
* Fix bug with showing data validation modal multiple times if app is reset.
* Allow replacing of uploaded data if there are data validation issues.
* Fix bug where auxiliary fields are already mapped (but mapping is not shown) when app is restarted.

# easypqt 0.3.1

* Fix bug in checking ReefCloud upload columns

# easypqt 0.3.0

* Add ReefCloud integration

# easypqt 0.2.1

* Allow different date formats, in case file has been opened in Excel etc

# easypqt 0.2.0

* Require CoralNet export that returns both shortcode and ID
* Move to using MERMAID's labelmappings endpoint instead of static file

# easypqt 0.1.5

* Add Google Analytics

# easypqt 0.1.4

* Handle case where user does not have any projects, show modal informing them to set up a project

# easypqt 0.1.3

* Add logo, update favicon

# easypqt 0.1.2

* Fix bug with duplicated records

# easypqt 0.1.1

* Allow for additional optional columns to be uploaded, identify auxiliary fields properly
* Clarify in error modal that Date field is invalid
* Fix bug where Date was not being used to construct Sample Unit
* Add modal while ingestion is running
