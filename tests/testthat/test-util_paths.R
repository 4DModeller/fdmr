test_that("clean_path - path gets cleaned correctly", {
  tilde_path <- "~/Documents/folder/test.txt"
  clean_tilde_path <- clean_path(path = tilde_path)

  expected <- fs::path(fs::path_home(), "Documents/folder/test.txt")

  expect_equal(clean_tilde_path, expected)
})

test_that("clean_path - file doesn't exist raises", {
  tilde_path <- "~/Documents/folder/test.txt"
  expect_error(clean_path(path = tilde_path, check_exists = TRUE))
})

test_that("clean_path - NULL or empty raises", {
  expect_error(clean_path(path = NULL))
  expect_error(clean_path(path = ""))
})
