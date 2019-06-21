context("utils")

test_that("reassign_function_body changes function body", {
    f1 <- function(x) x

    attr(f1, "a") <- 1

    environment(f1) <- new.env()

    f1_formals <- formals(f1)
    f1_env <- environment(f1)
    f1_attrs <- attributes(f1)

    new_body <- quote(x*x)

    expect_null(reassign_function_body(f1, new_body))

    expect_equal(body(f1), new_body)

    # check no side-effects
    expect_equal(formals(f1), f1_formals)
    expect_equal(environment(f1), f1_env)
    expect_equal(attributes(f1), f1_attrs)
})

test_that("reassign_function_body returns silently", {
    f1 <- function() 1
    expect_silent(reassign_function_body(f1, 2))
})

test_that("reassign_function_body returns invisibly", {
    f1 <- function() 1
    expect_invisible(reassign_function_body(f1, 2))
})

test_that("create_duplicate creates a duplicate", {
    f1 <- function(x) x
    f2 <- create_duplicate(f1)

    expect_false(sexp_address(f1) == sexp_address(f2))
    expect_true(identical(f1, f2))
    expect_true(compare(f1, f2)$equal)
})

test_that("sexp_address returns an address of SEXP", {
    a <- 1

    expect_equal(sexp_address(a), sexp_address(a))
    expect_true(startsWith(sexp_address(a), "0x"))
})
