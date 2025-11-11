#' Load traces of parameter types
#'
#' @param files If `TRUE`, return paths to all temporary files holding trace
#' data.
#' @param quiet If `FALSE`, issue message when no traces found.
#' @return A 'data.frame' of traces, including names of functions and
#' parameters, and values of each parameter traced in both unevaluated and
#' evaluated forms.
#' @export
#' @examples
#' f <- function (x, y, z, ...) {
#'     x * x + y * y
#' }
#' inject_tracer (f)
#' val <- f (1:2, 3:4 + 0., a = "blah")
#' x <- load_traces ()
#' print (x)
#'
#' # Traces should always be "uninjected":
#' uninject_tracer (f)
#' # Traces may also be removed:
#' clear_traces ()
load_traces <- function (files = FALSE, quiet = FALSE) {

    td <- get_typetrace_dir ()
    traces <- list.files (td, pattern = "^typetrace\\_", full.names = TRUE)

    if (length (traces) == 0L) {
        if (!quiet) {
            message ("No traces found; first run 'inject_tracer'")
        }
        return (NULL)
    }

    # These are 'meta'-level trace objects, which are moved into the main
    # function environment here, and removed from traces. Traces from that point
    # on may be analysed by iterating over sequences of parameter traces.
    fn_name <- par_formals <- num_traces <-
        trace_source <- call_envs <- NULL # nolint
    trace_objs <- c (
        "fn_name", "par_formals", "num_traces",
        "trace_source", "call_envs"
    )

    out <- lapply (traces, function (i) {

        tr_i <- readRDS (i)

        for (to in trace_objs) {
            assign (to, tr_i [[to]])
        }

        tr_i <- tr_i [which (!names (tr_i) %in% trace_objs)]
        fn_call_hash <- gsub ("^.*typetrace\\_|\\.Rds$", "", i)

        # simple vector columns:
        par_name <- vapply (tr_i, function (i) i$par, character (1L))
        types <- vapply (tr_i, function (i) i$type, character (1L))
        modes <- vapply (tr_i, function (i) i$mode, character (1L))
        storage_mode <- vapply (
            tr_i, function (i) i$storage_mode,
            character (1)
        )
        len <- vapply (tr_i, function (i) i$length, integer (1L))
        fmls <- par_formals [match (par_name, names (par_formals))]
        # list-columns:
        classes <- I (lapply (tr_i, function (i) i$class))
        par_uneval <- I (lapply (tr_i, function (i) i$par_uneval))
        par_eval <- I (lapply (tr_i, function (i) i$par_eval))

        if (nrow (call_envs) == 0L) {
            call_envs <- call_envs [1, ] # auto-fills with NA
        }
        call_envs$call_env <- paste0 (call_envs$namespace, "::", call_envs$name)
        call_envs$call_env [which (is.na (call_envs$name))] <- NA_character_

        out_i <- tibble::tibble (
            trace_name = i,
            trace_number = num_traces,
            trace_source = trace_source,
            fn_name = fn_name,
            fn_call_hash = fn_call_hash,
            call_env = call_envs$call_env,
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

        has_list <- integer (0L)
        if (get_trace_lists_param ()) {
            has_list <- which (vapply (
                tr_i,
                function (i) "list_data" %in% names (i),
                logical (1L)
            ))
        }

        if (length (has_list) > 0L) {

            out_list_i <- lapply (tr_i [has_list], function (j) {
                j_out <- do.call (rbind, lapply (j$list_data, as.data.frame))
                j_out$par <- paste0 (j$par, "$", j_out$par)
                return (j_out)
            })
            out_list_i <- do.call (rbind, out_list_i)
            names (out_list_i) [names (out_list_i) == "par"] <- "par_name"
            names (out_list_i) [names (out_list_i) == "par_uneval"] <- "uneval"
            names (out_list_i) [names (out_list_i) == "par_eval"] <- "eval"

            out_list <- out_i [integer (0L), ]
            out_list <- out_list [seq_len (nrow (out_list_i)), ]
            index <- match (names (out_list_i), names (out_list))
            out_list [, index] <- out_list_i
            index1 <- which (!names (out_list) %in% names (out_list_i))
            index2 <- match (names (out_list) [index1], names (out_i))
            out_list [, index1] <- out_i [seq_len (nrow (out_list_i)), index2]

            out_i <- rbind (out_i, out_list)
        }

        return (out_i)
    })

    out <- do.call (rbind, out)

    if (!files) {
        out$trace_name <- out$call_env <- NULL
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
#' @examples
#' f <- function (x, y, z, ...) {
#'     x * x + y * y
#' }
#' inject_tracer (f)
#' val <- f (1:2, 3:4 + 0., a = "blah")
#' x <- load_traces ()
#' print (x)
#'
#' # Then call 'clear_traces' to remove them:
#' clear_traces ()
#' # Trying to load again wil then indicate 'No traces found':
#' x <- load_traces ()
#' # Traces should also always be "uninjected":
#' uninject_tracer (f)
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
