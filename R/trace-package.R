
#' Trace all parameters for all functions in a specified package
#'
#' @param package Name of package to be traced (as character value).
#' @param pkg_dir For "types" including "tests", a local directory to the source
#' code of the package. (This is needed because installed versions do not
#' generally include tests.)
#' @param functions Optional character vector of names of functions to trace.
#' Defaults to tracing all functions.
#' @param types The types of code to be run to generate traces: one or both
#' values of "examples" or "tests" (as for `tools::testInstalledPackage`). Note
#' that only tests run via the \pkg{testthat} package can be traced.
#' @param trace_lists If `TRUE`, trace into any nested list parameters
#' (including `data.frame`-type objects), and return type information on each
#' list component. The parameter names for these list-components are then
#' specified in "dollar-notation", for example 'Orange$age'.
#' @return A `data.frame` of data on every parameter of every function as
#' specified in code provided in package examples.
#' @export
#' @examples
#' \dontrun{
#' res <- trace_package ("rematch")
#' res <- trace_package (pkg_dir = "/<path>/<to>/<local>/<pacakge>")
#' }
trace_package <- function (package = NULL,
                           pkg_dir = NULL,
                           functions = NULL,
                           types = c ("examples", "tests"),
                           trace_lists = FALSE) {

    types <- match.arg (types, c ("examples", "tests"),
        several.ok = TRUE
    )
    set_trace_list_option (trace_lists)

    package <- assert_trace_package_inputs (package, types, pkg_dir)
    pkg_was_attached <- any (grepl (paste0 ("package:", package), search ()))
    if (pkg_was_attached) {
        on.exit (tryCatch (
            attachNamespace (package),
            error = function (e) NULL
        ))
    }

    # -------- PRE_INSTALLATION
    lib_paths <- .libPaths ()
    lib_path <- pre_install (package, pkg_dir, quiet = FALSE)
    # Flag whether package was able to be pre-installed to local tempdir:
    pre_installed <- !lib_path %in% lib_paths
    if (pre_installed || is.null (pkg_dir)) {
        pkg_dir <- file.path (lib_path, package)
    }

    # -------- TRACING
    trace_fns <-
        inject_pkg_trace_fns (functions, package, trace_lists = trace_lists)

    traces_ex <- NULL

    if ("examples" %in% types) {
        trace_names <- trace_package_exs (package, functions)
        traces_ex <- list_traces ()
        add_trace_source (traces_ex, "examples")
    }
    if ("tests" %in% types) {
        if (testthat_is_parallel (pkg_dir) && !pre_installed) {
            message (
                "Tests run with testthat v3 in parallel can ",
                "not be traced, and will not be run."
            )
            test_traces <- NULL
        } else {
            test_traces <- trace_package_tests (package, pkg_dir, pre_installed)
            traces_test <- list_traces ()
            if (!is.null (traces_ex)) {
                traces_test <- traces_test [which (!traces_test %in% traces_ex)]
            }
            add_trace_source (traces_test, "tests")
        }
    }

    traces <- load_traces (files = TRUE, quiet = TRUE)

    if (!is.null (traces)) {

        traces <- add_pkg_trace_sources (
            traces,
            trace_names,
            test_traces,
            types
        )
    }

    uninject_pkg_trace_fns (trace_fns, package)

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

inject_pkg_trace_fns <- function (functions, package, trace_lists = FALSE) {

    clear_traces ()

    trace_fns <- functions
    p <- paste0 ("package:", package)
    if (is.null (trace_fns)) {
        trace_fns <- ls (p, all.names = TRUE)
    }

    pkg_env <- as.environment (p)
    for (fnm in trace_fns) {
        f <- get (fnm, envir = pkg_env)
        if (is.function (f)) {
            inject_tracer (f, trace_lists = trace_lists)
        }
    }

    return (trace_fns)
}

uninject_pkg_trace_fns <- function (trace_fns, package) {

    p <- paste0 ("package:", package)
    pkg_env <- as.environment (p)

    for (f in trace_fns) {
        f <- get (f, envir = pkg_env)
        if (is.function (f)) {
            uninject_tracer (f)
        }
    }

    # Envvar to enable traces to remain so that package can be used by
    # 'autotest', through loading traces after calling 'trace_package()'
    if (!Sys.getenv ("TYPETRACER_LEAVE_TRACES") == "true") {
        clear_traces ()
    }
}

#' Trace all examples from a package
#'
#' @param package Name of package to be traced.
#' @param functions Optional list of names of functions to be traced.
#' @return 'data.frame' of '.Rd' file names and trace names.
#' @noRd
trace_package_exs <- function (package, functions = NULL) {

    exs <- get_pkg_examples (package)

    if (is.null (exs)) {
        return ()
    }

    if (!is.null (functions)) {

        # Reduce examples down to only those which call specified functions
        has_functions <- vapply (exs, function (i) {
            p <- utils::getParseData (parse (text = i, keep.source = TRUE))
            fn_names <- p$text [p$token == "SYMBOL_FUNCTION_CALL"]
            any (functions %in% fn_names)
        }, logical (1L))

        exs <- exs [which (has_functions)]
    }

    if (length (exs) == 0L) {
        return (TRUE)
    }

    # suppress any plot output
    dev <- options ()$"device"
    options (device = NULL)

    # get current traces
    td <- get_typetrace_dir ()
    trace_list_old <- list.files (
        td,
        pattern = "^typetrace\\_",
        full.names = TRUE
    )

    # Evaluate each example separately, to avoid aborting evaluation process
    # when only one example errors
    traces <- lapply (exs, function (ex) {

        suppressWarnings ( # nolint - variable assigned but not used
            tryCatch ( # nolint - variable assigned but not used
                eval (parse (text = ex, keep.source = TRUE)),
                error = function (e) NULL
            )
        )
        trace_list_new <- list.files (
            td,
            pattern = "^typetrace\\_",
            full.names = TRUE
        )
        trace_list_added <-
            trace_list_new [which (!trace_list_new %in% trace_list_old)]
        trace_list_old <- trace_list_new

        return (trace_list_added)
    })

    options (device = dev)

    traces <- lapply (traces, function (i) data.frame (trace_name = i))
    traces <- do.call (rbind, traces) # inherits .Rd name as row name
    traces$rd_name <- gsub ("\\.[0-9]+$", "", rownames (traces))
    rownames (traces) <- NULL
    traces <- traces [, c ("rd_name", "trace_name")]

    return (traces)
}

# adapted from tools::testInstalledPackages
trace_package_tests <- function (package, pkg_dir = NULL,
                                 pre_installed = FALSE) {

    requireNamespace ("testthat")

    if (is.null (pkg_dir)) {
        return (list ()) # nocov
    }
    if (pre_installed) {
        insert_counters_in_tests (pkg_dir)
        if (testthat_is_parallel (pkg_dir)) {
            rm_testthat_parallel (pkg_dir)
        }
    }
    test_dir <- file.path (pkg_dir, "tests")

    if (!dir.exists (test_dir)) {
        return (list ()) # test_check returns list
    }

    out <- withr::with_dir (
        test_dir,
        testthat::test_package (package, reporter = testthat::ListReporter)
    )

    # `read_test_trace_numbers()` in @/load-traces.R
    test_trace_numbers <- read_test_trace_numbers (pkg_dir)

    if (nrow (test_trace_numbers) > 0L) {

        test_str <- lapply (out, function (i) c (i$file, i$test))
        test_str <- data.frame (do.call (rbind, test_str))
        names (test_str) <- c ("file", "test_name")
        test_str$file <- file.path (
            testthat::test_path (),
            test_str$file
        )
        test_str$test <- gsub ("\\s+", "_", test_str$test_name)
        index <- match (test_trace_numbers$test, test_str$test)
        test_trace_numbers$test_name <- test_str$test_name [index]
        test_trace_numbers$test_file <- test_str$file [index]
        test_trace_numbers$test <- NULL
        test_trace_numbers <-
            test_trace_numbers [, c ("test_file", "test_name", "trace_number")]
        rownames (test_trace_numbers) <- NULL
    }

    return (test_trace_numbers)
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

add_pkg_trace_sources <- function (traces, trace_names, test_traces, types) {

    traces <- tibble::add_column (
        traces,
        source_file_name = NA,
        .after = "trace_source"
    )

    if ("examples" %in% types) {
        # join rd_name from trace_names:
        trace_names$rd_name <- paste0 ("man/", trace_names$rd_name, ".Rd")
        index <- match (traces$trace_name, trace_names$trace)
        traces$source_file_name <- trace_names$rd_name [index]
    }
    if ("tests" %in% types && length (test_traces) > 0L) {
        traces <- join_test_trace_data (traces, test_traces)
    }
    traces$trace_name <- traces$trace_source <- NULL

    return (traces)
}

join_test_trace_data <- function (traces, test_traces) {

    if (!"trace_number" %in% names (test_traces) || nrow (test_traces) == 0L) {
        return (traces)
    }

    test_tr_start <- test_traces$trace_number
    test_tr_end <- c (
        test_traces$trace_number [-1] - 1,
        max (traces$trace_number, na.rm = TRUE)
    )
    if (any (is.na (test_tr_start)) || any (is.na (test_tr_end))) {
        return (traces)
    }

    tr_start1 <- min (test_tr_start, na.rm = TRUE)
    tr_end1 <- max (test_tr_end, na.rm = TRUE)
    if (length (tr_start1) == 0L || length (tr_end1) == 0L) {
        return (traces)
    }
    if (is.na (tr_start1) || is.na (tr_end1)) {
        return (traces)
    }

    test_names <- rep (
        test_traces$test_name,
        times = test_tr_end - test_tr_start + 1
    )
    test_files <- rep (
        test_traces$test_file,
        times = test_tr_end - test_tr_start + 1
    )
    test_tr_index <- seq (tr_start1, tr_end1)
    traces_index <- which (traces$trace_number %in% test_tr_index)
    index <- match (traces$trace_number [traces_index], test_tr_index)
    traces$source_file_name [traces_index] <-
        paste0 (test_files, "/", test_names) [index]

    return (traces)
}

add_trace_source <- function (traces, trace_source) {

    checkmate::assert_character (trace_source)

    for (i in traces) {
        tr <- readRDS (i)
        tr$trace_source <- trace_source
        saveRDS (tr, i)
    }
}
