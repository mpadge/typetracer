
#' Code injected in function heads that gets the types of all parameters
#'
#' This is currently only a prototype, and will not generally work
#' @noRd
get_types <- function () {

    # temp file to dump trace:
    td <- options ("typetracedir")
    nm <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    fname <- file.path (td, paste0 ("typetrace_", nm, ".txt"))

    # values:
    fn <- match.call () [[1]]
    pars <- formals (get (fn))

    for (p in names (pars)) {
        p_mode <- storage.mode (get (p))
        p_len <- length (get (p))
        out <- paste0 (c (fn, p, p_mode, p_len), collapse = ",")
        writeLines (out, fname)
    }
}
