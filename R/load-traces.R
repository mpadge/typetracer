
#' Load traces of parameter types
#'
#' @param quiet If `FALSE`, issue message when no traces found.
#' @export
load_traces <- function (quiet = FALSE) {

    td <- options ("typetracedir")$typetracedir
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
        tr_i <- tr_i [which (!names (tr_i) %in% c ("fn_name", "formals"))]

        fn_call_hash <- paste0 (sample (c (
            letters,
            LETTERS,
            0:9
        ),
        size = 8
        ),
        collapse = ""
        )

        # simple vector columns:
        par_name <- vapply (tr_i, function (i) i$par, character (1))
        storage_mode <- vapply (
            tr_i, function (i) {
                i$storage_mode
            },
            character (1)
        )
        len <- vapply (tr_i, function (i) i$length, integer (1))
        fmls <- par_formals [match (par_name, names (par_formals))]
        # list-columns:
        classes <- I (lapply (tr_i, function (i) i$class))
        par_uneval <- I (lapply (tr_i, function (i) i$par_uneval))
        par_eval <- I (lapply (tr_i, function (i) i$par_eval))

        tibble::tibble (
            trace_name = i,
            fn_name = fn_name,
            fn_call_hash = fn_call_hash,
            par_name = par_name,
            class = classes,
            storage_mode = storage_mode,
            length = len,
            formal = fmls,
            uneval = par_uneval,
            eval = par_eval
        )
    })

    out <- do.call (rbind, out)
    out <- out [order (out$fn_name), ]
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
#' @export
clear_traces <- function () {

    td <- options ("typetracedir")$typetracedir
    traces <- list.files (td, pattern = "^typetrace\\_", full.names = TRUE)

    invisible (file.remove (traces))
}
