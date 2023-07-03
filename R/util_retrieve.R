#' Retrieve the tutorial datasets and unpack them
#' to a folder in th user's home directory or a folder
#' specified in their use config file.
#'
#' @param dataset Name of dataset to retrieve
#' @param force_retrieval Force update of dataset information
#'
#' @return NULL
#' @export
retrieve_tutorial_data <- function(dataset, force_update = FALSE) {
  dataset <- base::tolower(dataset)

  download_cache_folder <- fs::path(fs::path_home(), "fdmr", "download_cache")

  retrieval_info_file <- fs::path(download_cache_folder, "metadata.json")
  dataset_info_file <- fs::path(download_cache_folder, "datasets.json")

  if (fs::file_exists(retrieval_info_file)) {
    retrieval_info <- jsonlite::read_json(retrieval_info_file)

    last_retrieved_str <- retrieval_info$datasets$datasets_retrieved
    last_retrieved <- lubridate::ymd_hms(last_retrieved_str)

    print(last_retrieved)

    

    retrieval_period <- lubridate::period(6, units = "hours")

    print(as.logical(lubridate::interval(last_retrieved, lubridate::now()) > retrieval_period))

    stop("waaa")

    if (retrieve) {
      # Retrieve the list of known datasets
      utils::download.file(url = "https://github.com/4DModeller/fdmr_data/raw/main/datasets.json", dataset_info_file)
      # Set the current time of retrieval and write out the JSON
      retrieval_info$datasets$datasets_retrieved <- lubridate::format_ISO8601(lubridate::now())
      jsonlite::write_json(retrieval_info, retrieval_info_file)
    }
  } else {
    # Retrieve the list of known datasets
    utils::download.file(url = "https://github.com/4DModeller/fdmr_data/raw/main/datasets.json", dataset_info_file)
    retrieval_info <- list("datasets" = list("retrieved" = lubridate::format_ISO8601(lubridate::now())))
    jsonlite::write_json(retrieval_info, path = retrieval_info_file)
  }

  extract_path <- fs::path(fs::path_home(), "fdmr", "tutorial_data", dataset)

  if (!fs::dir_exists(extract_path)) {
    fs::dir_create(extract_path, recurse = TRUE)
  }

  if (!fs::dir_exists(download_cache_folder)) {
    fs::dir_create(download_cache_folder, recurse = TRUE)
  }

  dataset_info <- jsonlite::read_json(dataset_info_file)
  available_datasets <- dataset_info$datasets

  if (dataset %in% available_datasets) {
    filename <- dataset_info[[dataset]]$filename
    download_path <- fs::path(download_cache_folder, filename)

    last_updated <- lubridate::ymd(dataset_info[[dataset]]$last_updated)
    mod_time <- fs::file_info(download_path)$modification_time

    needs_updating <- mod_time < last_updated

    if (!fs::file_exists(download_path) || needs_updating) {
      data_url <- base::paste0("https://github.com/4DModeller/fdmr_data/raw/main/", filename)
      utils::download.file(url = data_url, destfile = download_path)
    }
  } else {
    stop("Invalid dataset, please see available datasets at https://github.com/4DModeller/fdmr_data")
  }

  archive::archive_extract(download_path, dir = extract_path)
  base::cat("\nTutorial data extracted to ", extract_path, "\n")
}
