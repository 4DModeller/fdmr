#' Retrieves a tutorial dataset and unpacks it to a place specified by the user (~/fdmr/tutorial_data)
#'
#' @param dataset Name of dataset to retrieve
#' @param force_update Force retrieval of metadata and dataset
#' @param save Unpack the dataset to where user specified (character), home directory (TRUE), session's temporary directory (FALSE: default).
#'
#' @return NULL
#' @export
retrieve_tutorial_data <- function(dataset, force_update = FALSE, save = FALSE) {
  dataset <- base::tolower(dataset)

  if (base::isTRUE(save) | base::is.character(save)){
    if (base::is.character(save)) {
      save_path <- clean_path(save, check_exists = TRUE)
      download_cache_folder <- fs::path(save_path, "fdmr", "download_cache")
    } else{
      download_cache_folder <- fs::path(fs::path_home(), "fdmr", "download_cache")
    }
  } else {
    download_cache_folder <- fs::path(fs::path_temp(), "fdmr", "download_cache")
    }

  if (!fs::dir_exists(download_cache_folder)) {
    fs::dir_create(download_cache_folder, recurse = TRUE)
  }

  file_metadata_file <- fs::path(download_cache_folder, "metadata.json")
  dataset_info_file <- fs::path(download_cache_folder, "datasets.json")

  if (fs::file_exists(file_metadata_file)) {
    retrieval_info <- jsonlite::read_json(file_metadata_file)

    last_retrieved_str <- retrieval_info$datasets$retrieved
    last_retrieved <- lubridate::ymd_hms(last_retrieved_str)

    # We'll retrieve the list of datasets every 6 hours
    retrieval_period <- lubridate::period(6, units = "hours")
    retrieve <- lubridate::interval(lubridate::now(), last_retrieved) > retrieval_period

    if (retrieve || force_update) {
      # Retrieve the list of known datasets
      utils::download.file(url = "https://github.com/4DModeller/fdmr_data/raw/main/datasets.json", dataset_info_file)
      # Set the current time of retrieval and write out the JSON
      retrieval_info$datasets$datasets_retrieved <- lubridate::format_ISO8601(lubridate::now())
      jsonlite::write_json(retrieval_info, file_metadata_file)
    }
  } else {
    # Retrieve the list of known datasets
    utils::download.file(url = "https://github.com/4DModeller/fdmr_data/raw/main/datasets.json", dataset_info_file)
    retrieval_info <- list("datasets" = list("retrieved" = lubridate::format_ISO8601(lubridate::now())))
    jsonlite::write_json(retrieval_info, path = file_metadata_file)
  }

  if (base::isTRUE(save) | base::is.character(save)){
    if (base::is.character(save)){
      save_path <- clean_path(save, check_exists = TRUE)
      extract_path <- fs::path(save_path, "fdmr", "tutorial_data", dataset)
    } else{
      extract_path <- fs::path(fs::path_home(), "fdmr", "tutorial_data", dataset)
    }
  } else {
    extract_path <- fs::path(fs::path_temp(), "fdmr", "tutorial_data", dataset)
    }
  

  if (!fs::dir_exists(extract_path)) {
    fs::dir_create(extract_path, recurse = TRUE)
  }

  dataset_info <- jsonlite::read_json(dataset_info_file)
  available_datasets <- dataset_info$datasets

  if (dataset %in% names(available_datasets)) {
    filename <- available_datasets[[dataset]]$filename
    download_path <- fs::path(download_cache_folder, filename)

    last_updated <- lubridate::ymd(available_datasets[[dataset]]$last_updated)
    mod_time <- fs::file_info(download_path)$modification_time

    # TODO - this might not work on Windows, check
    needs_updating <- mod_time < last_updated
    if (length(needs_updating) == 0) {
      needs_updating <- TRUE
    }

    if (force_update || !fs::file_exists(download_path) || needs_updating) {
      data_url <- base::paste0("https://github.com/4DModeller/fdmr_data/raw/main/", filename)
      utils::download.file(url = data_url, destfile = download_path)
    }

    utils::untar(download_path, exdir = extract_path)
    base::cat("\nTutorial data extracted to ", extract_path, "\n")
  } else {
    stop("Invalid dataset, please see available datasets at https://github.com/4DModeller/fdmr_data")
  }
}