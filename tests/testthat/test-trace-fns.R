
is_gh_cov <- identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage")


test_that ("tracer body", {

    body <- body (get_types)
    if (!is_gh_cov) {
        # covr injects other symbols into code on workflow, so snapshot differs
        testthat::expect_snapshot (body)
    }
})

test_that ("injected tracer body", {

    f <- function (x, y) {
        x * x + y * y
    }
    body0 <- body (f)

    inject_tracer (f)
    body1 <- body (f)

    expect_false (identical (body0, body1))
    expect_true (length (body1) > length (body0))

    expect_equal (body1 [[2]], body (get_types))
})

test_that ("No traces", {

    clear_traces ()
    expect_message (
        x <- load_traces (),
        "No traces found; first run 'inject_tracer'"
    )
    expect_null (x)
})

test_that ("trace call", {

    f <- function (x, y) {
        x * x + y * y
    }

    inject_tracer (f)

    clear_traces ()
    val <- f (x = 1:2, y = 3:4 + 0.)
    flist <- list.files (tempdir (),
        pattern = "^typetrace\\_",
        full.names = TRUE
    )
    expect_true (length (flist) > 0L)

    x <- load_traces (files = TRUE)

    expect_s3_class (x, "tbl_df")
    expect_equal (nrow (x), 2L) # x and y
    expect_equal (ncol (x), 13L)
    expect_identical (
        names (x),
        c (
            "trace_name", "trace_number", "fn_name",
            "fn_call_hash", "par_name", "class",
            "typeof", "mode", "storage_mode", "length",
            "formal", "uneval", "eval"
        )
    )
})

test_that ("untrace call", {

    f <- function (x, y) {
        x * x + y * y
    }
    body0 <- body (f)

    inject_tracer (f)
    body1 <- body (f)

    expect_true (uninject_tracer (f))
    body2 <- body (f)

    e0 <- as.character (as.expression (body0))
    e1 <- as.character (as.expression (body1))
    e2 <- as.character (as.expression (body2))

    expect_false (identical (e0, e1))

    expect_identical (e0, e2)
})
