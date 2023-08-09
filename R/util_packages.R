#' Checks if packages are installed.
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
