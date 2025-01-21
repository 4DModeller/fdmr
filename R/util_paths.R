#' Return the filepath for a tutorial data file.
#'
#' @param dataset Name of dataset
#' @param filename Name of file
#' @param saved Specify the location where user unpacked the data.
#'
#' @return fs::path Full filepath
#' @export
get_tutorial_datapath <- function(dataset, filename, saved = FALSE) {
  tutorial_datapath <- fs::path(get_tutorial_cache_datapath(saved), dataset)

  if (!fs::dir_exists(tutorial_datapath)) {
    stop("Unable to load data, the folder ", toString(tutorial_datapath), " does not exist. Have you run retrieve_tutorial_data?")
  }

  glob_str <- paste0("*/", toString(filename))
  fpath <- fs::dir_ls(path = tutorial_datapath, glob = glob_str, recurse = TRUE)

  if (length(fpath) == 0) {
    stop(paste("Unable to find", filename, "please check.\n"))
  }
  return(fpath)
}


#' Load data from the tutorial data store
#'
#' @param dataset Name of dataset
#' @param filename Name of file
#' @param saved Specify the location where user unpacked the data
#'
#' @return loaded object
#' @export
load_tutorial_data <- function(dataset, filename, saved = FALSE) {
  if (!tolower(fs::path_ext(filename)) == "rds") {
    stop("We can only load rds files.")
  }

  fpath <- get_tutorial_datapath(dataset = dataset, filename = filename, saved = saved)
  return(readRDS(fpath))
}


#' Returns a ~ expanded absolute path
#'
#' @param path Path to clean
#' @param check_exists Check if the path exists, error if it doesn't
#'
#' @return fs::path Expanded absolute path
#' @keywords internal
clean_path <- function(path, check_exists = FALSE) {
  if (is.null(path) || nchar(path) == 0) {
    stop("Invalid path of zero length given.")
  }

  fpath <- fs::path_abs(fs::path_expand(path))

  if (check_exists) {
    if (!fs::file_exists(fpath)) {
      stop(paste("The path", fpath, "does not exist."))
    }
  }

  return(fpath)
}


#' Get path to tutorial data cache folder
#' 
#' @param saved Specify the location where user unpacked the data
#'
#' @return fs::path
#' @keywords internal
get_tutorial_cache_datapath <- function(saved) {
  if (base::isTRUE(saved) | base::is.character(saved)){
    if (base::is.character(saved)) {
      save_path <- check_path(saved)
      fs::path(save_path, "fdmr", "tutorial_data")
    } else{
      fs::path(fs::path_home(), "fdmr", "tutorial_data")
    }
  } else {
    fs::path(fs::path_temp(), "fdmr", "tutorial_data")
  }
}

#' Get path to downloaded archive cache folder
#' 
#' @param saved Specify the location where user unpacked the data
#'
#' @return fs::path
#' @keywords internal
get_archive_cache_datapath <- function(saved) {
  if (base::isTRUE(saved) | base::is.character(saved)){
    if (base::is.character(saved)) {
      save_path <- check_path(saved)
      fs::path(save_path, "fdmr", "download_cache")
    } else{
      fs::path(fs::path_home(), "fdmr", "download_cache")
    }
  } else {
    fs::path(fs::path_temp(), "fdmr", "download_cache")
  }
}


#' Clear both tutorial data and downloaded archive caches
#'
#' @param saved Specify the location where user unpacked the data
#'
#' @return NULL
#' @export
clear_caches <- function(saved = TRUE) {
  tut_path <- get_tutorial_cache_datapath(saved)
  cache_path <- get_archive_cache_datapath(saved)
  print(paste("Deleting ", tut_path, cache_path))
  fs::dir_delete(tut_path)
  fs::dir_delete(cache_path)
}


#' Get the system temporary directory
#'
#' @return NULL
#' @keywords internal
get_tmpdir <- function() {
  fs::path_dir(fs::path_temp())
}
