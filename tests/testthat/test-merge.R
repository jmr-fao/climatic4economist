test_that("find_merge_var finds shared columns of two data frames", {
    df1 <- data.frame(ID = 1:3, Value = c(10, 20, 30))
    df2 <- data.frame(ID = c(2, 3, 4), Other = c(200, 300, 400))
    expect_equal(find_merge_var(df1, df2), "ID")
})

test_that("find_merge_var reduces over a list of data frames", {
    lst <- list(data.frame(ID = 1, a = 1, b = 1),
                data.frame(ID = 2, a = 2, c = 2),
                data.frame(ID = 3, a = 3, d = 3))
    expect_equal(find_merge_var(lst), c("ID", "a"))
})

test_that("find_merge_var returns an empty vector when nothing is shared", {
    expect_length(find_merge_var(data.frame(a = 1), data.frame(b = 2)), 0L)
})

test_that("merge_by_common joins on the shared column", {
    df1 <- data.frame(ID = 1:3, x = c(10, 20, 30))
    df2 <- data.frame(ID = c(2, 3, 4), y = c(200, 300, 400))
    out <- merge_by_common(df1, df2)
    expect_equal(nrow(out), 4L)             # full join
    expect_setequal(names(out), c("ID", "x", "y"))
})

test_that("merge_with_survey joins new values onto the survey by ID", {
    survey <- data.frame(ID = c("1", "2"), hh = c("a", "b"))
    values <- data.frame(ID = c("1", "2"), spi = c(0.5, -1.2))
    out <- merge_with_survey(survey, values)
    expect_equal(nrow(out), 2L)
    expect_setequal(names(out), c("ID", "hh", "spi"))
})

test_that("merge_with_survey drops pre-existing date columns from the survey", {
    survey <- data.frame(ID = "1", hh = "a", `2020.01` = 99, check.names = FALSE)
    values <- data.frame(ID = "1", `2020.01` = 1.5, check.names = FALSE)
    out <- merge_with_survey(survey, values)
    # the survey's own date column is dropped, so no .x/.y suffixes appear
    expect_false(any(grepl("\\.x$|\\.y$", names(out))))
    expect_equal(out[["2020.01"]], 1.5)
})

test_that("compute_water_balance adds precipitation and evapotranspiration", {
    pre <- data.frame(ID = 1:2, x_cell = c(10, 20), y_cell = c(40, 50),
                      `2020` = c(100, 120), check.names = FALSE)
    pet <- data.frame(ID = 1:2, x_cell = c(10, 20), y_cell = c(40, 50),
                      `2020` = c(-50, -60), check.names = FALSE)
    out <- compute_water_balance(pre, pet)
    expect_equal(out[["2020"]], c(50, 60))
    expect_true(all(c("ID", "x_cell", "y_cell") %in% names(out)))
})

test_that("prepare_coord assigns one ID per unique coordinate pair", {
    df <- data.frame(lat = c(1, 2, 1, 2), lon = c(10, 20, 10, 20),
                     v = 1:4)
    out <- prepare_coord(df, lat, lon)
    expect_true("ID" %in% names(out))
    expect_equal(dplyr::n_distinct(out$ID), 2L)
    # identical coordinates share an ID
    expect_equal(length(unique(out$ID[out$lat == 1])), 1L)
})
