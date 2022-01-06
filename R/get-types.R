
#' Code injected in function heads that gets the types of all parameters
#'
#' This is currently only a prototype, and will not generally work
#' @noRd
get_types <- function () {

    # temp file to dump trace:
    td <- options ("typetracedir")
    nm <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    fname <- file.path (td, paste0 ("typetrace_", nm, ".txt"))
    typetracer_con <- file (fname, open = "at")

    # Extract values. `match.call` returns the *expressions* submitted to the
    # call, while the evaluated versions of formalArgs are stored in the
    # environment. `get` is used for the latter to avoid re-`eval`-ing, but
    # `...` args are not eval'd on function entry.
    fn_call <- match.call (expand.dots = TRUE)
    fn_name <- fn_call [[1]]
    pars <- as.list (fn_call [-1L]) # unevalated expressions
    par_names <- names (pars)
    fn_env <- environment ()

    for (p in par_names) {
        if (p %in% names (fn_env)) {
            p_eval <- get (p, envir = fn_env)
        } else if (p %in% names (pars)) {
            p_eval <- eval (pars [[p]])
        }
        p_mode <- storage.mode (p_eval)
        p_len <- length (p_eval)
        out <- paste0 (c (fn_name, p, p_mode, p_len), collapse = ",")
        writeLines (out, typetracer_con)
    }
    close (typetracer_con)

    rm (td, nm, fname, typetracer_con,
        fn_call, fn_name, pars, par_names,
        fn_env, p, p_eval, p_mode, p_len, out)
}
