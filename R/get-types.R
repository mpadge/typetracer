
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
    fn_env <- environment ()

    fn <- match.fun (fn_name)
    par_names <- methods::formalArgs (fn)

    get_p <- function (p, fn_env) {
        tryCatch (
            get (p, envir = fn_env, inherits = FALSE),
            error = function (e) NULL)
    }
    eval_p <- function (p, pars, fn_env) {
        tryCatch (
            eval (pars [[p]], envir = fn_env),
            error = function (e) NULL)
    }

    classes <- vapply (par_names, function (p) {

                            res <- get_p (p, fn_env)
                            if (is.null (res)) {
                                res <- eval_p (p, pars, fn_env)
                            }

                            c (class (res) [1],
                               storage.mode (res),
                               length (res))
            }, character (3))

    classes <- data.frame (t (classes))
    colnames (classes) <- c ("class", "storage.mode", "length")
    classes$fn_name <- as.character (fn_name)
    classes$p <- par_names

    classes <- classes [, c ("fn_name", "p", "class", "storage.mode", "length")]
    apply (classes, 1, function (i) {
                          out <- paste0 (i, collapse = ",")
                          writeLines (out, typetracer_con)
            })
    close (typetracer_con)

    rm (td, nm, fn, fname, typetracer_con,
        fn_call, fn_name, fn_env, pars, par_names,
        get_p, eval_p, classes)
}
