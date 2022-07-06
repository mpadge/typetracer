
#' Trace all parameters for all functions in a specified package
#'
#' @param package Name of package to be traced (as character value)
#' @param functions Optional character vector of names of functions to trace.
#' Defaults to tracing all functions.
#' @param types The types of code to be run to generate traces: one or both
#' values of "examples" or "tests" (as for `tools::testInstalledPackage`).
#' @param pkg_dir For "types" including "tests", a local directory to the source
#' code of the package. (This is needed because installed versions do not
#' generally include tests.)
#' @return A `data.frame` of data on every parameter of every function as
#' specified in code provided in package examples.
#' @export
trace_package <- function (package = NULL,
                           functions = NULL,
                           types = c ("examples", "tests"),
                           pkg_dir = NULL) {

    package <- assert_trace_package_inputs (package, types, pkg_dir)
    pkg_was_attached <- any (grepl (paste0 ("package:", package), search ()))
    if (pkg_was_attached) {
        on.exit (attachNamespace (package))
    }

    # -------- PRE_INSTALLATION
    lib_path <- pre_install (package, pkg_dir, quiet = FALSE)
    if (is.null (pkg_dir)) {
        pkg_dir <- file.path (lib_path, package)
    }

    # -------- TRACING
    # The original `functions = NULL` has to be passed through to
    # `trace_package_exs`, so modified here as `trace_fns`:
    trace_fns <- functions
    p <- paste0 ("package:", package)
    if (is.null (trace_fns)) {
        trace_fns <- ls (p, all.names = TRUE)
    }
    pkg_env <- as.environment (p)
    for (f in trace_fns) {
        f <- get (f, envir = pkg_env)
        if (is.function (f)) {
            inject_tracer (f)
        }
    }

    clear_traces ()

    if ("examples" %in% types) {
        check <- trace_package_exs (package, functions) # dummy logical value
    }
    if ("tests" %in% types) {
        check <- trace_package_tests (package, pkg_dir)
    }

    traces <- load_traces (quiet = TRUE)

    clear_traces ()

    for (f in trace_fns) {
        f <- get (f, envir = pkg_env)
        if (is.function (f)) {
            uninject_tracer (f)
        }
    }

    tryCatch (
        unloadNamespace (package),
        error = function (e) NULL
    )
    check <- reload_pkg (package, lib_path)

    return (traces)
}

assert_trace_package_inputs <- function (package = NULL,
                                         types = c ("examples", "tests"),
                                         pkg_dir = NULL) {

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

    return (package)
}

trace_package_exs <- function (package, functions = NULL) {

    exs <- get_pkg_examples (package)

    if (is.null (exs)) {
        return ()
    }

    if (!is.null (functions)) {

        # Reduce examples down to only those which call specified functions
        has_functions <- vapply (exs, function (i) {
            p <- utils::getParseData (parse (text = i))
            fn_names <- p$text [p$token == "SYMBOL_FUNCTION_CALL"]
            any (functions %in% fn_names)
        }, logical (1L))

        exs <- exs [which (has_functions)]
    }

    # suppress any plot output
    dev <- options ()$"device"
    options (device = NULL)

    # Evaluate each example separately, to avoid aborting evaluation process
    # when only one example errors
    out <- lapply (exs, function (ex) {

        suppressWarnings ( # nolint - variable assigned but not used
            tryCatch ( # nolint - variable assigned but not used
                eval (parse (text = ex)),
                error = function (e) NULL
            )
        )
    })

    options (device = dev)

    return (TRUE)
}

# adapted from tools::testInstalledPackages
trace_package_tests <- function (package, pkg_dir = NULL) {

    requireNamespace ("testthat")
    requireNamespace ("withr")

    if (is.null (pkg_dir)) {
        return (list ()) # nocov
    }
    test_dir <- file.path (pkg_dir, "tests")

    if (!dir.exists (test_dir)) {
        return (list ()) # test_check returns list
    }

    withr::with_dir (
        test_dir,
        testthat::test_check (package)
    )

    return (TRUE)
}

get_pkg_examples <- function (package) {

    rd <- tools::Rd_db (package)

    if (length (rd) == 0L && any (grepl (package, search ()))) {
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
