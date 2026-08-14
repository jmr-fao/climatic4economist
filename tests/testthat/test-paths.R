test_that("check_path returns the path when every level exists", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data", "geo"), recursive = TRUE)
    writeLines("x", file.path(root, "data", "geo", "file.txt"))

    expect_equal(check_path(root, "data", "geo", "file.txt"),
                 file.path(root, "data", "geo", "file.txt"))
})

test_that("check_path reports the first missing component", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data"))

    expect_error(check_path(root, "data", "missing_folder", "file.txt"),
                 "Path does not exist")
    expect_error(check_path(root, "data", "missing_folder", "file.txt"),
                 "missing_folder")
})

test_that("check_path can list the contents of the last valid directory", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data"))
    writeLines("x", file.path(root, "data", "present.txt"))

    expect_error(check_path(root, "data", "absent.txt", list_contents = TRUE),
                 "present.txt")
})

test_that("find_root_directory walks up to a matching folder", {
    root <- withr::local_tempdir()
    nested <- file.path(root, "anchor", "a", "b")
    dir.create(nested, recursive = TRUE)

    found <- find_root_directory(start = nested, root_directories = "anchor")
    expect_equal(basename(found), "anchor")
})

test_that("find_root_directory matches case-insensitively by default", {
    root <- withr::local_tempdir()
    nested <- file.path(root, "Anchor", "a")
    dir.create(nested, recursive = TRUE)

    expect_equal(basename(find_root_directory(start = nested,
                                              root_directories = "anchor")),
                 "Anchor")
    expect_error(find_root_directory(start = nested,
                                     root_directories = "anchor",
                                     ignore_case = FALSE))
})

test_that("find_root_directory reports the folder it was looking for", {
    # regression: the error path referenced an undefined `root_directory`,
    # so a failed search crashed with "object not found"
    root <- withr::local_tempdir()
    expect_error(
        find_root_directory(start = root, root_directories = "no_such_anchor"),
        "no_such_anchor"
    )
})

test_that("find_subfolder locates a nested folder by name", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data", "weather"), recursive = TRUE)

    found <- find_subfolder(root, "weather")
    expect_equal(basename(found), "weather")
})

test_that("find_subfolder returns NULL when nothing matches", {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "data"))
    expect_null(find_subfolder(root, "nothing_like_this"))
})

test_that("find_subfolder errors on a missing root", {
    expect_error(find_subfolder(file.path(tempdir(), "definitely_absent"), "x"),
                 "Root directory does not exist")
})
