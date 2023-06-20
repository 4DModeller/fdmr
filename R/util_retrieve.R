#' Retrieve the tutorial datasets and unpack them
#' to a folder in th user's home directory or a folder
#' specified in their use config file.
#'
#' @param dataset Name of dataset to retrieve, either covid or hydro
#' @return NULL
#' @export
retrieve_tutorial_data <- function(dataset, filename) {
  dataset <- base::tolower(dataset)
  # TODO - update this to retrieve a JSON/YAML of available tutorial
  # datasets
  if (dataset == "covid") {
    filename <- "covid19_data.tar.bz2"
  } else if (dataset == "hydro") {
    filename <- "hydro_data.tar.bz2"
  } else if (dataset == "covid_mcmc") {
    filename <- "covid19_data2.tar.bz2"
  } else {
    stop("Invalid dataset name.")
  }

  download_cache_folder <- fs::path(fs::path_home(), "fdmr", "download_cache")
  # We extract each dataset into its own directory in case we have the same filenames
  # in different datasets
  extract_path <- fs::path(fs::path_home(), "fdmr", "tutorial_data", dataset)

  download_path <- fs::path(download_cache_folder, filename)

  if (!fs::dir_exists(extract_path)) {
    fs::dir_create(extract_path, recurse = TRUE)
  }

  if (!fs::dir_exists(download_cache_folder)) {
    fs::dir_create(download_cache_folder, recurse = TRUE)
  }

  # Check the cache file
  if (!fs::file_exists(download_path)) {
    data_url <- base::paste0("https://github.com/4DModeller/fdmr_data/raw/main/", filename)
    utils::download.file(url = data_url, destfile = download_path)
  }

  archive::archive_extract(download_path, dir = extract_path)
  base::cat("\nTutorial data extracted to ", extract_path, "\n")
}
