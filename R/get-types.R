
#' Code injected in function heads that gets the types of all parameters
#'
#' All variables are defined within a new environment, to avoid any confusion
#' with variables defined within functions in which this code in injected, and
#' to enable all of the local variables defined here to be easily deleted once
#' types have been traced. This environment also has to have an unambiguous and
#' unique name.
#' @noRd
get_types <- function () {

    tt_env <- new.env (parent = emptyenv ())

    # temp file to dump trace:
    tt_env$td <- options ("typetracedir")
    tt_env$nm <- paste0 (sample (c (letters, LETTERS), 8),
                                 collapse = "")
    tt_env$fname <- file.path (tt_env$td,
        paste0 ("typetrace_", tt_env$nm, ".txt"))
    tt_env$typetracer_con <- file (tt_env$fname, open = "at")

    # Extract values. `match.call` returns the *expressions* submitted to the
    # call, while the evaluated versions of formalArgs are stored in the
    # environment. `get` is used for the latter to avoid re-`eval`-ing, but
    # `...` args are not eval'd on function entry.
    tt_env$fn_call <- match.call (expand.dots = TRUE)
    tt_env$fn_name <- tt_env$fn_call [[1]]
    tt_env$pars <- as.list (tt_env$fn_call [-1L])

    fn_env <- environment ()

    tt_env$fn <- match.fun (tt_env$fn_name)
    tt_env$par_names <- methods::formalArgs (tt_env$fn)

    tt_env$classes <- vapply (tt_env$par_names, function (p) {

        res <- NULL

        if (p %in% ls (fn_env)) {
            res <- tryCatch (
                get (p, envir = fn_env, inherits = FALSE),
                error = function (e) NULL)
        }

        if (is.null (res)) {
            res <- tryCatch (
                eval (tt_env$pars [[p]], envir = fn_env),
                error = function (e) NULL)
        }

        c (class (res) [1],
           storage.mode (res),
           length (res))

    }, character (3))

    tt_env$classes <- data.frame (t (tt_env$classes))
    colnames (tt_env$classes) <- c ("class", "storage.mode", "length")
    tt_env$classes$fn_name <- as.character (tt_env$fn_name)
    tt_env$classes$p <- tt_env$par_names

    tt_env$cols <- c ("fn_name", "p", "class", "storage.mode", "length")
    tt_env$classes <- tt_env$classes [, tt_env$cols]

    apply (tt_env$classes, 1, function (i) {
                          writeLines (paste0 (i, collapse = ","),
                                      tt_env$typetracer_con)
            })
    close (tt_env$typetracer_con)

    rm (tt_env)
}
