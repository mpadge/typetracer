
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
                       xi <- brio::readLines (i)
                       do.call (rbind, strsplit (xi, ","))
        })
    out <- do.call (rbind, out)
    out <- tibble::tibble ("function"  = out [, 1],
                           "parameter" = out [, 2],
                           "storage_mode" = out [, 3],
                           "length" = as.integer (out [, 4]))

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
