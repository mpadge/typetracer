
#' Load traces of parameter types
#'
#' @param files If `TRUE`, return paths to all temporary files holding trace
#' data.
#' @param quiet If `FALSE`, issue message when no traces found.
#' @return A 'data.frame' of traces, including names of functions and
#' parameters, and values of each parameter traced in both unevaluated and
#' evaluated forms.
#' @export
load_traces <- function (files = FALSE, quiet = FALSE) {

    td <- get_typetrace_dir ()
    traces <- list.files (td, pattern = "^typetrace\\_", full.names = TRUE)

    if (length (traces) == 0L) {
        if (!quiet) {
            message ("No traces found; first run 'inject_tracer'")
        }
        return (NULL)
    }

    out <- lapply (traces, function (i) {

        tr_i <- readRDS (i)

        fn_name <- tr_i$fn_name
        par_formals <- tr_i$formals
        num_traces <- tr_i$num_traces
        tr_i <- tr_i [which (!names (tr_i) %in%
            c ("fn_name", "formals", "num_traces"))]
        fn_call_hash <- gsub ("^.*typetrace\\_|\\.Rds$", "", i)

        # simple vector columns:
        par_name <- vapply (tr_i, function (i) i$par, character (1L))
        types <- vapply (tr_i, function (i) i$type, character (1L))
        modes <- vapply (tr_i, function (i) i$mode, character (1L))
        storage_mode <- vapply ( # wrapped coz otherwise > 80 char wide
            tr_i, function (i) {
                i$storage_mode
            },
            character (1)
        )
        len <- vapply (tr_i, function (i) i$length, integer (1L))
        fmls <- par_formals [match (par_name, names (par_formals))]
        # list-columns:
        classes <- I (lapply (tr_i, function (i) i$class))
        par_uneval <- I (lapply (tr_i, function (i) i$par_uneval))
        par_eval <- I (lapply (tr_i, function (i) i$par_eval))

        tibble::tibble (
            trace_name = i,
            trace_number = num_traces,
            fn_name = fn_name,
            fn_call_hash = fn_call_hash,
            par_name = par_name,
            class = classes,
            typeof = types,
            mode = modes,
            storage_mode = storage_mode,
            length = len,
            formal = fmls,
            uneval = par_uneval,
            eval = par_eval
        )
    })

    out <- do.call (rbind, out)

    if (!files) {
        out$trace_name <- NULL
    }

    out <- out [order (out$trace_number), ]
    rownames (out) <- NULL

    names (out$par_name) <- NULL
    names (out$formal) <- names (out$uneval) <- names (out$eval) <- out$par_name

    return (out)
}

#' Clear previous traces
#'
#' Traces are by default appended to previous traces. This function can be used
#' to clean those previous ones, to enable subsequent calls to generate new
#' traces that are not appended to previous ones.
#'
#' @return (Invisibly) A single logical value indicating whether or not traces
#' were successfully cleared.
#' @export
clear_traces <- function () {

    td <- get_typetrace_dir ()
    traces <- list.files (td, pattern = "^typetrace\\_", full.names = TRUE)

    clear_fn_bodies_dir ()

    invisible (file.remove (traces))
}

#' Read numbers of traces at start of each test
#'
#' These numbers are created by the code injected by the
#' `insert_counters_in_tests()` function in install.R.
#'
#' @param install_path file.path (typetracedir, package), passed here as single
#' string.
#' @return A `data.frame` of test names and number of traces at the start of
#' that test. These numbers can then be used to associate traces with the
#' specified test names in the "source" column from `load_traces()`.
#' @noRd
read_test_trace_numbers <- function (install_path) {

    flist <- list.files (
        install_path,
        pattern = "^tracetest\\_",
        full.names = TRUE
    )
    num_traces <- data.frame (trace_number = vapply (
        flist,
        function (i) as.integer (readLines (i) [1]),
        integer (1L)
    ))
    num_traces$test <- gsub (
        "^tracetest\\_|\\.txt$|\\.txt$",
        "",
        basename (rownames (num_traces))
    )
    num_traces <-
        num_traces [order (num_traces$trace_number), c ("test", "trace_number")]
    rownames (num_traces) <- NULL

    return (num_traces)
}
