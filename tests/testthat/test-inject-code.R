context("test-inject-code")

test_that("inject_code injects code at the beginning of the given function body", {
    f <- function(x) {
        1+1
        42
    }

    capture <- NULL

    expect_null(inject_code(f, capture <<- x))

    expect_equal(f(1), 42)
    expect_equal(capture, 1)
})

test_that("inject_code injects code to be run at function exit", {
    capture <<- NULL

    f <- function(x) {
        capture <<- 1
        42
    }

    expect_null(inject_code(f, capture <<- x, "onexit"))

    expect_equal(f(3), 42)
    expect_equal(capture, 3)
})

test_that("inject_code injects code to be run when function throws an exception", {
    capture <- NULL

    f <- function(x) {
        capture <<- 1
        stop("err")
    }

    expect_null(inject_code(f, capture <<- x, "onerror"))

    expect_error(f(3))
    expect_equal(capture, 3)
})

test_that("inject_code supports multiple injection", {
    capture <- numeric(0)

    f <- function(x) {
        capture <<- c(capture, 1)
        if (x) stop("err")
        else 42
    }

    expect_null(inject_code(f, capture <<- c(capture, 0), "onentry"))
    expect_null(inject_code(f, capture <<- c(capture, 2), "onexit"))
    expect_null(inject_code(f, capture <<- c(capture, 3), "onerror"))

    expect_error(f(TRUE))
    expect_equal(capture, c(0, 1, 3, 2))

    capture <- numeric(0)

    expect_equal(f(FALSE), 42)
    expect_equal(capture, c(0, 1, 2))
})

test_that("inject_code returns silently", {
    f <- function() 1
    expect_silent(inject_code(f, 2))
})

test_that("inject_code returns invisibly", {
    f <- function() 1
    expect_invisible(inject_code(f, 2))
})
