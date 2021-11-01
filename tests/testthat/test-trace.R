test_that("tracer body", {

    body <- body (get_types)
    testthat::expect_snapshot (body)
})
