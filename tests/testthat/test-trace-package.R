
is_gh_cov <- identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage")

test_that ("errors", {
    package <- "rematch"
    pkg_dir <- file.path (tempdir (), "does_not_exist")
    expect_error (
        trace_package (package, pkg_dir = pkg_dir),
        "Assertion on 'pkg_dir' failed"
    )
})


test_that ("trace installed package", {

    # test pkg selected based on smallest installed size plus latest update >
    # 2015 or so. "praise" is also an option
    package <- "rematch"

    expect_s3_class (x0 <- trace_package (package), "tbl_df")

    expect_true (nrow (x0) > 5) # arbitrarily low number
    expect_identical (
        names (x0),
        c (
            "fn_name", "fn_call_hash", "par_name",
            "class", "storage_mode", "length",
            "formal", "uneval", "eval"
        )
    )

    expect_s3_class (
        x1 <- trace_package (package,
            types = c ("examples", "tests")
        ),
        "tbl_df"
    )
    expect_true (nrow (x1) > 5)
    expect_identical (
        names (x1),
        c (
            "fn_name", "fn_call_hash", "par_name",
            "class", "storage_mode", "length",
            "formal", "uneval", "eval"
        )
    )

    # installed packages have no tests, so traces are examples only:
    expect_identical (nrow (x0), nrow (x1))
})

test_that ("trace source package", {

    # note that this is from the source
    # https://github.com/MangoTheCat/rematch
    # which is different from the CRAN version, and includes `re_match_all()`.
    # Running tests only works with the version installed via devtools.
    # It's not worth adding that huge pkg to 'Suggests' just to increase test
    # coverage by a couple of lines.
    tarball <- testthat::test_path ("..", "rematch_1.0.1.tar.gz")
    skip_if (!file.exists (tarball))

    if (utils::untar (tarball, exdir = tempdir (), tar = "internal") != 0) {
        stop ("Unable to extract tarball to 'tempdir'")
    }

    package <- "rematch"
    path <- normalizePath (file.path (tempdir (), package))
    requireNamespace (package)

    expect_s3_class (
        x0 <- trace_package (package, pkg_dir = path, types = "examples"),
        "tbl_df"
    )

    expect_true (nrow (x0) > 5) # arbitrarily low number
    expect_identical (
        names (x0),
        c (
            "fn_name", "fn_call_hash", "par_name",
            "class", "storage_mode", "length",
            "formal", "uneval", "eval"
        )
    )
})
