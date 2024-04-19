read_copy <- function(key) {
  yaml::read_yaml(app_sys("copy.yml"))[[key]]
}

close_button <- shiny::modalButton("Close")
