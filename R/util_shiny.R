#' Get a busy spinner for our Shiny apps.
#' If we have an internet connection we download the spinner
#' TODO - add this gif to the package
#'
#' @return busyspinner
#' @keywords internal
get_busy_spinner <- function() {
  if (curl::has_internet()) {
    busy_spinner <- shinybusy::add_busy_gif("https://raw.githubusercontent.com/4DModeller/logo/main/4DMlogo_loading.gif", height = 100, width = 100, position = "top-right")
  } else {
    busy_spinner <- shinybusy::add_busy_spinner(spin = "folding-cube", margins = c(20, 20))
  }
  busy_spinner
}
