test_that("ensure_filepath_writable - we can definitely write to the path", {
    if (.Platform$OS.type == "unix") {
        path <- fs::path_norm(fs::path("/etc/fdmr/logs.txt"))
    } else {
        path <- fs::path_norm(fs::path("C:/Users/logs.txt"))
    }

    writable_path <- ensure_filepath_writable(path)

    writable_parent <- fs::path_dir(writable_path)
    expect_equal(as.numeric(file.access(writable_parent)), 0)
    tmpdir <- get_tmpdir()
    expect_equal(writable_parent, tmpdir)
})
