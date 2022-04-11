
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
        xi <- lapply (strsplit (xi, ","), function (i) {
            if (length (i) > 6L) {
                index <- seq_along (i) [-(1:5)]
                i [6] <- paste0 (i [index], collapse = ",")
                i <- i [1:6]
            }
            return (i)
            })
        do.call (rbind, xi)
    })
    out <- do.call (rbind, out)
    out <- tibble::tibble ("fn_name"  = out [, 1],
                           "par_name" = out [, 2],
                           "class" = out [, 3],
                           "storage_mode" = out [, 4],
                           "length" = as.integer (out [, 5]),
                           "par_uneval" = out [, 6])

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
