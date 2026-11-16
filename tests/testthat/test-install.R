test_that ("insert_counters_in_tests handles ordinary test descriptions", {

    # Reproduces https://github.com/mpadge/typetracer/issues/29.

    # Windows temp paths routinely contain "\U" (as in "\Users\"), which breaks parsing once spliced unescaped into the generated "ftmp" line.
    skip_on_os ("windows")

    pkg_dir <- file.path (tempdir (), "faketypetracerpkg")
    test_path <- file.path (pkg_dir, "tests", "testthat")
    dir.create (test_path, recursive = TRUE, showWarnings = FALSE)
    withr::defer (unlink (pkg_dir, recursive = TRUE))

    test_file <- file.path (test_path, "test-tricky.R")
    writeLines (
        c (
            "test_that (\"act_funs() accepts a named activation\", {",
            "    expect_true (TRUE)",
            "})",
            "",
            "test_that (\"validate_activation() errors when it doesn't exist\", {",
            "    expect_true (TRUE)",
            "})",
            "",
            "test_that (\"aborts when {pkgname} isn't installed\", {",
            "    expect_true (TRUE)",
            "})",
            "",
            "test_that (\"a test with no brace block\", expect_true (TRUE))"
        ),
        test_file
    )

    withr::local_options (list (typetracedir = tempdir ()))
    expect_silent (typetracer:::insert_counters_in_tests (pkg_dir))

    expect_error (parse (test_file), NA)

    injected <- readLines (test_file)
    expect_identical (length (grep ("^traces <- list\\.files \\(", injected)), 3L)
    expect_identical (length (grep ("expect_true", injected)), 4L)
})

test_that ("insert_counters_in_tests handles '/' in test descriptions", {

    skip_on_os ("windows") # same reason as above

    pkg_dir <- file.path (tempdir (), "faketypetracerpkg2")
    test_path <- file.path (pkg_dir, "tests", "testthat")
    dir.create (test_path, recursive = TRUE, showWarnings = FALSE)
    withr::defer (unlink (pkg_dir, recursive = TRUE))

    test_file <- file.path (test_path, "test-slash.R")
    writeLines (
        c (
            "test_that (\"lm/limma agree\", {",
            "    expect_true (TRUE)",
            "})"
        ),
        test_file
    )

    withr::local_options (list (typetracedir = tempdir ()))
    typetracer:::insert_counters_in_tests (pkg_dir)

    # The counter file must sit directly in 'pkg_dir', not in a
    # non-existent "tracetest_lm" sub-directory:
    injected <- readLines (test_file)
    ftmp_line <- grep ("^ftmp <- ", injected, value = TRUE)
    ftmp <- eval (parse (text = sub ("^ftmp <- ", "", ftmp_line)))
    expect_identical (dirname (ftmp), pkg_dir)
    expect_identical (basename (ftmp), "tracetest_lm_limma_agree.txt")

    # 'trace_package()' matches counter files back to the test name
    # reported by testthat:
    expect_identical (
        typetracer:::test_name_for_file ("lm/limma agree"),
        "lm_limma_agree"
    )
})
