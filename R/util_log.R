#' Write to a logfile
#'
#' @param filepath Path to log
#' @param message Message to write
#'
#' @return NULL
#' @keywords internal
write_log <- function(filepath, message) {
  filepath <- ensure_filepath_writable(filepath)
  exists <- fs::file_exists(filepath)
  message <- paste0("[", lubridate::format_ISO8601(lubridate::now()), "] - ", message, "\n")
  tryCatch(
    expr = {
      cat(message, file = filepath, append = exists)
    },
    error = function(cond) {
      warning("Unable to write log to file ", cond)
    }
  )
}


#' Write a parameter set to JSON
#'
#' @param filepath Path to parameter file
#' @param message Parameters to write
#'
#' @return NULL
#' @keywords internal
write_parameters <- function(filepath, parameters) {
  filepath <- ensure_filepath_writable(filepath)
  tryCatch(
    expr = {
      write(jsonlite::toJSON(parameters), file = filepath)
    },
    error = function(cond) {
      warning("Unable to parameters to file ", cond)
    }
  )
}

#' Checks if we can write to the given filepath
#' and if we can't updates it to use the system
#' temporary directory
#'
#' @param filepath Path to file

#' @return fs::path
#' @keywords internal
ensure_filepath_writable <- function(filepath) {
  if (!as.numeric(file.access(filepath)) == 0) {
    filename <- fs::path_file(filepath)
    warning("Unable to write to ", filepath)
    filepath <- fs::path(get_tmpdir(), filename)
    warning("Updating path to ", filepath)
  }
  filepath
}
