#' Write to a logfile
#'
#' @param filepath Path to log
#' @param message Message to write
#'
#' @return NULL
#' @keywords internal
write_log <- function(filepath, message) {
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

#' A thin wrapper to write to an RDS file but also
#' checks to make sure we can write out to the filepath
#'
#' @param filepath Path to log
#' @param data Data to write
#' @param compress Compress the data
#'
#'
#' @return NULL
#' @keywords internal
write_rds <- function(filepath, data, compress = TRUE) {
  tryCatch(
    expr = {
      saveRDS(data, filepath, compress = compress)
    },
    error = function(cond) {
      warning("Unable to write rds file ", cond)
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
  tryCatch(
    expr = {
      write(jsonlite::toJSON(parameters), file = filepath)
    },
    error = function(cond) {
      warning("Unable to parameters to file ", cond)
    }
  )
}
