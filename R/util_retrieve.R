#' Retrieve the tutorial datasets and unpack them
#' to a folder in th user's home directory or a folder
#' specified in their use config file.
#'
#' @param tarball_path Path to tutorial data tarball (name to be changed)
#' @param data_store Folder to store extracted tutorial data
#' @return NULL
#' @export
retrieve_tutorial_data <- function(tarball_path, data_store = NULL) {
  warning("This function will be modified to retrieve data from a URL once we've got everything public.")

  if (!base::is.null(data_store)) {
    stop("Not implemented error.")
  }

  extract_path <- fs::path(fs::path_home(), "fdmr", "tutorial_data")

  if (!fs::dir_exists(extract_path)) {
    fs::dir_create(extract_path, recurse = TRUE)
  }

  tarball_path <- fs::path_expand(tarball_path)
  archive::archive_extract(tarball_path, dir = extract_path)
  cat("\nTutorial data extracted to ", extract_path, "\n")
}
