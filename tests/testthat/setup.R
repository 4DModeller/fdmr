local_temp_output_dir <- function() {
  tmp_dir <- fs::path(fs::path_temp(), "fdmr_test_out")
  fs::dir_create(tmp_dir)
  return(tmp_dir)
}

#' Return the full absolute filepath to a local test data file.
#' If the file is compressed using bz2 it will be extracted first
#' to a temporary folder and the path to the decompressed filepath
#' will be returned.
#'
#'
#' @param filename Name of file in 4d_modeller_test_data repo
#' @param folder_name Folder containing filename
#'
#' @return fs::path / list: Filepath or list of filepaths
#' @export
#'
#' @examples
local_get_test_datapath <- function(filename, folder_name) {
  data_folder <- fs::path_real(testthat::test_path("data"))

  full_filepath <- fs::path(data_folder, folder_name, filename)

  if (fs::path_ext(filename) == "bz2") {
    root_tmp_dir <- local_temp_output_dir()
    rand_str <- stringi::stri_rand_strings(1, length = 8)[1]
    extract_tmp_dir <- fs::path(root_tmp_dir, rand_str)
    fs::dir_create(extract_tmp_dir)

    utils::untar(full_filepath, exdir = extract_tmp_dir)

    folder_contents <- fs::dir_ls(extract_tmp_dir)

    return(folder_contents)
  }

  return(full_filepath)
}
