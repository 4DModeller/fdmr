#' Retrieve the tutorial datasets and unpack them
#' to a folder in th user's home directory or a folder
#' specified in their use config file.
#'
#' @param dataset Name of dataset to retrieve, either covid or hydro
#' @return NULL
#' @export
retrieve_tutorial_data <- function(dataset, data_store = NULL) {
  dataset <- base::tolower(dataset)
  if (dataset == "covid") {
    filename <- "covid19_data.tar.bz2"
  } else if (dataset == "hydro") {
    filename <- "hydro_data.tar.bz2"
  } else {
    stop("Invalid dataset name.")
  }

  cache_folder <- fs::path(fs::path_home(), "fdmr", "download_cache")
  extract_path <- fs::path(fs::path_home(), "fdmr", "tutorial_data")

  download_path <- fs::path(cache_folder, filename)

  if (!fs::dir_exists(extract_path)) {
    fs::dir_create(extract_path, recurse = TRUE)
  }

  if (!fs::dir_exists(cache_folder)) {
    fs::dir_create(cache_folder, recurse = TRUE)
  }

  # Check the cache file
  if (!fs::file_exists(download_path)) {
    data_url <- base::paste0("https://github.com/4DModeller/fdmr_data/raw/main/", filename)
    utils::download.file(url = data_url, destfile = download_path)
  }

  archive::archive_extract(download_path, dir = extract_path)
  base::cat("\nTutorial data extracted to ", extract_path, "\n")
}
