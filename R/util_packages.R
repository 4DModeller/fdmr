#' Checks if a package is installed. If it is it's loaded,
#' otherwise stop is called with a message telling the user
#' the package is required.
#'
#' @param packages Package name
#'
#' @return NULL
#' @keywords internal
require_packages <- function(packages) {
  if (!is.vector(packages)) {
    packages <- as.list(packages)
  }

  missing_packages <- list()
  for (package in packages) {
    if (package %in% rownames(utils::installed.packages()) == FALSE) {
      missing_packages <- append(missing_packages, package)
    }
  }

  if (length(missing_packages) > 0) {
    stop(paste("Missing package(s):", paste(missing_packages, collapse = ", "), ". Please install before continuing."))
  }
}
