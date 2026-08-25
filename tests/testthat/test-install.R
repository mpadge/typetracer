test_that ("insert_counters_in_tests handles ordinary test descriptions", {

    # Reproduces https://github.com/mpadge/typetracer/issues/29: descriptions
    # containing regex metacharacters ("foo() does x"), apostrophes/quotes
    # ("doesn't"), or a literal brace ("{pkgname} isn't installed") used to
    # make `insert_counters_in_tests()` crash with
    # `Error in seq.default() : 'from' must be a finite number`, because it
    # located the test body via a text `grep()` of the description instead
    # of via real parse-token positions.

    pkg_dir <- file.path (tempdir (), "faketypetracerpkg")
    test_path <- file.path (pkg_dir, "tests", "testthat")
    dir.create (test_path, recursive = TRUE, showWarnings = FALSE)
    withr::defer (unlink (pkg_dir, recursive = TRUE))

    test_file <- file.path (test_path, "test-tricky.R")
    writeLines (
        c (
            'test_that ("act_funs() accepts a named activation", {',
            "    expect_true (TRUE)",
            "})",
            "",
            'test_that ("validate_activation() errors when it doesn\'t exist", {',
            "    expect_true (TRUE)",
            "})",
            "",
            'test_that ("aborts when {pkgname} isn\'t installed", {',
            "    expect_true (TRUE)",
            "})"
        ),
        test_file
    )

    withr::local_options (list (typetracedir = tempdir ()))
    expect_silent (typetracer:::insert_counters_in_tests (pkg_dir))

    expect_error (parse (test_file), NA)

    injected <- readLines (test_file)
    expect_identical (length (grep ("^traces <- list\\.files \\\(", injected)), 3L)
    expect_identical (length (grep ("expect_true", injected)), 3L)
})
