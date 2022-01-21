
#' Trace all parameters for all functions in a specified package
#'
#' @param package Name of package to be traced (as character value)
#' @param types The types of code to be run to generate traces: one or both
#' values of "examples" or "tests" (as for `tools::testInstalledPackage`).
#' @param pkg_dir For "types" including "tests", a local directory to the source
#' code of the package. (This is needed because installed versions do not
#' generally include tests.)
#' @return A `data.frame` of data on every parameter of every function as
#' specified in code provided in package examples.
#' @export
trace_package <- function (package = NULL,
                           types = c ("examples", "tests"),
                           pkg_dir = NULL) {

    types <- match.arg (types, c ("examples", "tests"),
                        several.ok = TRUE)

    checkmate::assert_character (package)
    checkmate::assert_scalar (package)
    if (!is.null (pkg_dir)) {
        checkmate::assert_character (pkg_dir)
        checkmate::assert_scalar (pkg_dir)
        if (!checkmate::check_directory_exists (pkg_dir)) {
            stop ("Directory [", pkg_dir, "] does not exist")
        }
    }

    p <- paste0 ("package:", package)
    pkg_attached <- p %in% search ()
    if (!pkg_attached) {
        attachNamespace (package)
    }

    fns <- ls (p, all.names = TRUE)
    pkg_env <- as.environment (p)
    for (f in fns) {
        f <- get (f, envir = pkg_env)
        if (is.function (f)) {
            inject_tracer (f)
        }
    }

    clear_traces ()

    if ("examples" %in% types) {
        res <- trace_package_exs (package)
    }
    if ("tests" %in% types) {
        res <- c (res,
                  trace_package_tests (package, pkg_dir))
    }

    traces <- load_traces (quiet = TRUE)

    clear_traces ()

    for (f in fns) {
        f <- get (f, envir = pkg_env)
        if (is.function (f)) {
            uninject_tracer (f)
        }
    }

    unloadNamespace (package)
    if (pkg_attached) {
        attachNamespace 
    }

    return (traces)
}

trace_package_exs <- function (package) {

    exs <- get_pkg_examples (package)

    # suppress any plot output
    dev <- options ()$"device"
    options (device = NULL)
    o <- suppressWarnings (
        out <- tryCatch (
            eval (parse (text = exs)),
            error = function (e) NULL)
    )
    options (device = dev)
}

# adapted from tools::testInstalledPackages
trace_package_tests <- function (package, pkg_dir = NULL) {

    requireNamespace ("testthat")
    requireNamespace ("withr")

    if (is.null (pkg_dir)) {
        ip <- utils::installed.packages ()
        ip <- ip [ip [, 1] == package, ]
        pkg_dir <- file.path (ip [which (names (ip) == "LibPath")],
                              package)
    }
    if (length (pkg_dir) == 0L) { # in test environments
        return (list ())
    }
    test_dir <- file.path (pkg_dir, "tests")

    if (!dir.exists (test_dir)) {
        return (list ()) # test_check returns list
    }

    withr::with_dir (test_dir,
                     testthat::test_check (package))
}

get_pkg_examples <- function (package) {

    rd <- tools::Rd_db (package)

    has_exs <- vapply (rd, function (i) {
                out <- vapply (i, function (j)
                        any (attr (j, "Rd_tag") == "\\examples"),
                        logical (1))
                any (out)
    }, logical (1))

    exs <- lapply (rd [which (has_exs)], function (i) {
        f <- tempfile ()
        tools::Rd2ex (i, out = f)
        out <- brio::read_lines (f)
        file.remove (f)
        return (out)
    })

    exs <- unname (do.call (c, exs))

    return (exs)
}
