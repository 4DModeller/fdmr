#' Write to a logfile
#'
#' @param logfile Path to log
#' @param message Message to write
#'
#' @return NULL
#' @keywords internal
write_log <- function(logfile, message) {
  exists <- fs::file_exists(logfile)
  message <- paste0("[", lubridate::format_ISO8601(lubridate::now()), "] - ", message, "\n")
  cat(message, file = logfile, append = exists)
}


#' Write a parameter set to JSON
#'
#' @param logfile Path to parameter file
#' @param message Parameters to write
#'
#' @return NULL
#' @keywords internal
write_parameters <- function(logfile, parameters) {
  write(jsonlite::toJSON(parameters), file = logfile)
}
