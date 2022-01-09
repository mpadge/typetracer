
is_gh_cov <- identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage")


test_that("trace package", {

    package <- "checkmate"

    expect_s3_class (x <- trace_package (package), "tbl_df")

    expect_true (nrow (x) > 1000)
    expect_identical (names (x),
                      c ("function", "parameter", "storage_mode", "length"))
})
