#' Removes any character that isn't a letter
#'
#' @param data String data
#'
#' @return string data
#' @keywords internal
numbers_only <- function(data) {
  stringr::str_replace_all(data, pattern = "[A-Za-z]", replacement = "")
}
