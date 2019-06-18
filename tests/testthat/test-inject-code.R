context("test-inject-code")

test_that("inject_code injects code (LANGSXP) at the beginning of the given function body", {
    f <- function(x) { 42 }

    capture <- NULL

    expect_null(inject_code(capture <<- x, f))

    expect_equal(f(1), 42)
    expect_equal(capture, 1)
})

test_that("inject_code injects code (LANGSXP) at the beginning of the given function body with no {}", {
    f <- function(x) 42

    capture <- NULL

    expect_null(inject_code(capture <<- x, f))

    expect_equal(f(1), 42)
    expect_equal(capture, 1)
})

test_that("inject_code injects code (LANGSXP) at the beginning of the given function body with ...", {
    f <- function(...) 42

    capture <- NULL

    expect_null(inject_code(capture <<- c(...), f))

    expect_equal(f(1, 2, 3), 42)
    expect_equal(capture, c(1, 2, 3))
})

test_that("inject_code injects code (LANGSXP from symbol) at the beginning of the given function body", {
    f <- function(x) {
        42
    }

    capture <- NULL

    inject <- function(code) {
        expect_null(inject_code(code, f))
    }

    inject(quote(capture <<- x))

    expect_equal(f(1), 42)
    expect_equal(capture, 1)
})

test_that("inject_code injects code (EXPRSXP from symbol) at the beginning of the given function body", {
    f <- function(x) {
        42
    }

    capture <- NULL

    inject <- function(code) {
        expect_null(inject_code(code, f))
    }

    inject(expression(capture <<- x))

    expect_equal(f(1), 42)
    expect_equal(capture, 1)
})

test_that("inject_code injects code (CLOSXP from symbol) at the beginning of the given function body", {
    f <- function(x) {
        1+1
        42
    }

    capture <- NULL

    g <- function(y) {
        capture <<- y
    }

    inject <- function(code) {
        expect_null(inject_code(code, f))
    }

    inject(g)
    expect_equal(f(2), 42)
    expect_equal(capture, 2)
})

test_that("inject_code injects code (CLOSXP with no args from symbol) at the beginning of the given function body", {
    f <- function() {
        1+1
        42
    }

    capture <- NULL

    g <- function() {
        capture <<- 42
    }

    inject <- function(code) {
        expect_null(inject_code(code, f))
    }

    inject(g)
    expect_equal(f(), 42)
    expect_equal(capture, 42)
})

test_that("inject_code injects code to be run at function exit", {
    capture <<- NULL

    f <- function(x) {
        capture <<- 1
        42
    }

    expect_null(inject_code(capture <<- x, f, "onexit"))

    expect_equal(f(3), 42)
    expect_equal(capture, 3)
})

test_that("inject_code injects code to be run at function exit getting return value", {
    capture <<- NULL

    f <- function(x) {
        capture <<- 1
        42
    }

    expect_null(inject_code(capture <<- returnValue(), f, "onexit"))

    expect_equal(f(3), 42)
    expect_equal(capture, 42)
})

test_that("inject_code injects code to be run when function throws an exception", {
    capture <- NULL

    f <- function(x) {
        capture <<- 1
        if (x) stop("err")
        else 42
    }

    expect_null(inject_code(capture <<- 2, f, "onerror"))

    expect_equal(f(FALSE), 42)
    expect_equal(capture, 1)

    expect_error(f(TRUE))
    expect_equal(capture, 2)
})

test_that("inject_code injects code to be run when function exists successfully", {
    capture <- NULL

    f <- function(x) {
        capture <<- 1
        if (x) stop("err")
        else 42
    }

    expect_null(inject_code(capture <<- 2, f, "onsuccess"))

    expect_error(f(TRUE))
    expect_equal(capture, 1)

    expect_equal(f(FALSE), 42)
    expect_equal(capture, 2)
})

test_that("inject_code supports multiple injection", {
    capture <- numeric(0)

    f <- function(x) {
        capture <<- c(capture, 1)
        if (x) stop("err")
        else 42
    }

    expect_null(inject_code(capture <<- c(capture, 0), f, "onentry"))
    expect_null(inject_code(capture <<- c(capture, 2), f, "onexit"))
    expect_null(inject_code(capture <<- c(capture, 3), f, "onerror"))

    expect_error(f(TRUE))
    expect_equal(capture, c(0, 1, 3, 2))

    capture <- numeric(0)

    expect_equal(f(FALSE), 42)
    expect_equal(capture, c(0, 1, 2))
})

test_that("inject_code returns silently", {
    f <- function() 1
    expect_silent(inject_code(2, f))
})

test_that("inject_code returns invisibly", {
    f <- function() 1
    expect_invisible(inject_code(2, f))
})

test_that("inject_code wraps code into a function to avoid variable clashes", {
    f <- function(x) x
    g <- function(x) x

    expect_null(inject_code(x <- 2, f, wrap=FALSE))
    expect_equal(f(1), 2)

    expect_null(inject_code(x <- 4, g, wrap=TRUE))
    expect_equal(g(2), 2)
})
