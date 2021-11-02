
#' Code injected in function heads that gets the types of all parameters
#'
#' This is currently only a prototype, and will not generally work
#' @noRd
get_types <- function () {

    # temp file to dump trace:
    td <- options ("typetracedir")
    nm <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    fname <- file.path (td, paste0 ("typetrace_", nm, ".txt"))
    con <- file (fname, open = "w")

    # Extract values. `match.call` returns the *expressions* submitted to the
    # call, while the evaluated versions are stored in the environment. `get` is
    # used for the latter to avoid re-`eval`-ing.
    fn_call <- match.call (expand.dots = TRUE)
    fn_name <- fn_call [[1]]
    pars <- as.list (fn_call [-1L]) # unevalated expressions
    par_names <- names (pars)

    for (p in par_names) {
        p_eval <- get (p, pos = -1L)
        p_mode <- storage.mode (p_eval)
        p_len <- length (p_eval)
        out <- paste0 (c (fn_name, p, p_mode, p_len), collapse = ",")
        writeLines (out, con)
    }
    close (con)
}
