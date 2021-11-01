test_that("tracer body", {

    body <- body (get_types)
    testthat::expect_snapshot (body)
})

test_that("injected tracer body", {

    f <- function (x, y) {
        x * x + y * y
    }
    body0 <- body (f)

    inject_tracer (f, .GlobalEnv)
    body1 <- body (f)

    expect_false (identical (body0, body1))
    expect_true (length (body1) > length (body0))

    expect_equal (body1 [[2]], body (get_types))
})

test_that("No traces", {

    expect_message (x <- load_traces (),
                    "No traces found; first run 'inject_tracer'")
    expect_null (x)
})

test_that("trace call", {
    f <- function (x, y) {
        x * x + y * y
    }
    inject_tracer (f, .GlobalEnv)

    clear_traces ()
    val <- f (x = 1:2, y = 3:4 + 0.)
    flist <- list.files (tempdir (),
                         pattern = "^typetrace\\_",
                         full.names = TRUE)
    expect_true (length (flist) > 0L)

    x <- load_traces ()

    expect_s3_class (x, "tbl_df")
    expect_equal (nrow (x), 2L) # x and y
    expect_equal (ncol (x), 4L)
    expect_identical (names (x),
                      c ("function", "parameter", "storage_mode", "length"))
})
