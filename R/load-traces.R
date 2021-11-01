
#' Load traces of parameter types
#'
#' @export
load_traces <- function () {

    td <- options ("typetracedir")$typetracedir
    traces <- list.files (td, pattern = "^typetrace\\_", full.names = TRUE)

    if (length (traces) == 0L) {
        message ("No traces found; first run 'inject_tracer'")
        return (NULL)
    }

    out <- lapply (traces, function (i) {
                       xi <- brio::readLines (i)
                       do.call (rbind, strsplit (xi, ","))
        })
    out <- tibble::tibble (data.frame (do.call (rbind, out)))

    names (out) <- c ("function", "parameter", "storage_mode", "length")

    out$length <- as.integer (out$length)

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
