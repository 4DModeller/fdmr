#' Convert vector of date strings to Date objects
#'
#' @param time_data Time data to sort
#' @param sort Sort if TRUE
#' @param date_format Date format to pass to lubridate::as_date
#'
#' @return vector
#' @export
to_dates <- function(time_data, sort = FALSE, date_format = NULL) {
  dates <- lubridate::as_date(time_data)
  if (sort) {
    dates <- base::sort(dates)
  }
  return(dates)
}
