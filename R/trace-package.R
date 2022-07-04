
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

    # -------- ASSERTIONS
    types <- match.arg (types, c ("examples", "tests"),
        several.ok = TRUE
    )

    if (!is.null (pkg_dir)) {

        checkmate::assert_character (pkg_dir)
        checkmate::assert_scalar (pkg_dir)
        checkmate::assert_directory_exists (pkg_dir)

        if (is.null (package)) {
            package <- pkg_name_from_desc (pkg_dir)
        }
    }

    checkmate::assert_character (package)
    checkmate::assert_scalar (package)

    # -------- PRE_INSTALLATION
    libs <- .libPaths ()
    if (!is.null (pkg_dir)) {
        install_path <- pre_install (pkg_dir)
        libs <- c (install_path, libs)
    }

    p <- paste0 ("package:", package)
    pkg_attached <- p %in% search ()
    if (!pkg_attached) {
        lib_path <- tryCatch (
            find.package (package, lib.loc = libs),
            error = function (e) NULL
        )
        if (is.null (lib_path)) {
            stop (
                "Package '", package, "' is not installed. Please ",
                "install locally, or use 'devtools::load_all()' ",
                "before calling 'trace_package()'",
                call. = FALSE
            )
        }
        attachNamespace (package)
    }

    # -------- TRACING
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
        res <- c (
            res,
            trace_package_tests (package, pkg_dir)
        )
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

    return (traces)
}

trace_package_exs <- function (package) {

    exs <- get_pkg_examples (package)
    exs <- unname (do.call (c, exs))

    if (is.null (exs)) {
        return ()
    }

    # suppress any plot output
    dev <- options ()$"device"
    options (device = NULL)
    o <- suppressWarnings (
        out <- tryCatch (
            eval (parse (text = exs)),
            error = function (e) NULL
        )
    )
    options (device = dev)
}

# adapted from tools::testInstalledPackages
trace_package_tests <- function (package, pkg_dir = NULL) {

    requireNamespace ("testthat")
    requireNamespace ("withr")

    if (is.null (pkg_dir)) {
        e <- as.environment (paste0 ("package:", package))
        pkg_dir <- attr (e, "path")
    }
    if (length (pkg_dir) == 0L) { # in test environments
        return (list ())
    }
    test_dir <- file.path (pkg_dir, "tests")

    if (!dir.exists (test_dir)) {
        return (list ()) # test_check returns list
    }

    withr::with_dir (
        test_dir,
        testthat::test_check (package)
    )
}

get_pkg_examples <- function (package) {

    rd <- tools::Rd_db (package)

    if (length (rd) == 0L & any (grepl (package, search ()))) {
        # local load via devtools
        # This can't be tested because 'package' is in Suggests, which means it
        # has `Rd_db` entries.
        # nocov start
        e <- as.environment (paste0 ("package:", package))
        path <- attr (e, "path")

        if (is.null (path)) {
            return (NULL)
        }
        if (!dir.exists (path)) {
            return (NULL)
        }

        man_files <- list.files (
            file.path (path, "man"),
            full.names = TRUE,
            pattern = "\\.Rd$"
        )
        rd <- lapply (man_files, tools::parse_Rd)
        # nocov end
    }

    has_exs <- vapply (rd, function (i) {
        out <- vapply (
            i, function (j) {
                any (attr (j, "Rd_tag") == "\\examples")
            },
            logical (1)
        )
        any (out)
    }, logical (1))

    exs <- lapply (rd [which (has_exs)], function (i) {
        f <- tempfile ()
        tools::Rd2ex (i, out = f)
        out <- brio::read_lines (f)
        file.remove (f)
        return (out)
    })

    nm_ptn <- "^\\#\\#\\#\\sName\\:\\s"
    nms <- vapply (
        exs, function (i) {
            gsub (
                nm_ptn, "",
                grep (nm_ptn, i, value = TRUE) [1]
            )
        },
        character (1)
    )
    names (exs) <- nms

    return (exs)
}
