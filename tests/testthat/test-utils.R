
test_that ("reassign_function_body changes function body", {

    f1 <- function (x) x

    attr (f1, "a") <- 1

    environment (f1) <- new.env ()

    f1_formals <- formals (f1)
    f1_env <- environment (f1)
    f1_attrs <- attributes (f1)

    new_body <- quote (x * x)

    expect_null (reassign_function_body (f1, new_body))

    expect_equal (body (f1), new_body)

    # check no side-effects
    expect_equal (formals (f1), f1_formals)
    expect_equal (environment (f1), f1_env)
    expect_equal (attributes (f1), f1_attrs)
})

test_that ("reassign_function_body returns silently", {

    f1 <- function () 1
    expect_silent (reassign_function_body (f1, 2))
})

test_that ("reassign_function_body returns invisibly", {

    f1 <- function () 1
    expect_invisible (reassign_function_body (f1, 2))
})

test_that ("package not installed error", {
    expect_error  (trace_package (package = "abc123"),
                  "Package 'abc123' is not installed.")
})
