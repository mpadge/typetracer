
is_gh_cov <- identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage")


test_that("trace package", {

    package <- "checkmate"

    expect_s3_class (x0 <- trace_package (package), "tbl_df")

    expect_true (nrow (x0) > 50) # arbitrarily low number
    expect_identical (names (x0),
                      c ("function", "parameter", "storage_mode", "length"))

    expect_s3_class (x1 <- trace_package (package,
                                          types = c ("examples", "tests")),
                     "tbl_df")
    expect_true (nrow (x1) > 50)
    expect_identical (names (x1),
                      c ("function", "parameter", "storage_mode", "length"))

    # installed packages have no tests, so traces are examples only:
    expect_identical (nrow (x0), nrow (x1))
})
