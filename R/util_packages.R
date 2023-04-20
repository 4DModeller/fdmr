#' Checks if a package is installed. If it is it's loaded,
#' otherwise stop is called with a message telling the user
#' the package is required.
#'
#' @param name Package name
#'
#' @return NULL
#' @keywords internal
require_package <- function(pkg_name) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    stop(paste("The package", pkg_name, "is required by this function. Please install."))
  }
}
