
#' Code injected in function heads that gets the types of all parameters
#'
#' All variables are defined within a new environment, to avoid any confusion
#' with variables defined within functions in which this code in injected, and
#' to enable all of the local variables defined here to be easily deleted once
#' types have been traced. This environment also has to have an unambiguous and
#' unique name.
#' @noRd
get_types <- function () {

    typetracer_env <- new.env (parent = emptyenv ())

    # temp file to dump trace:
    typetracer_env$td <- options ("typetracedir")
    typetracer_env$nm <- paste0 (sample (c (letters, LETTERS), 8),
                                 collapse = "")
    typetracer_env$fname <- file.path (typetracer_env$td,
        paste0 ("typetrace_", typetracer_env$nm, ".txt"))
    typetracer_env$typetracer_con <- file (typetracer_env$fname, open = "at")

    # Extract values. `match.call` returns the *expressions* submitted to the
    # call, while the evaluated versions of formalArgs are stored in the
    # environment. `get` is used for the latter to avoid re-`eval`-ing, but
    # `...` args are not eval'd on function entry.
    typetracer_env$fn_call <- match.call (expand.dots = TRUE)
    typetracer_env$fn_name <- typetracer_env$fn_call [[1]]
    typetracer_env$pars <- as.list (typetracer_env$fn_call [-1L])

    fn_env <- environment ()

    typetracer_env$fn <- match.fun (typetracer_env$fn_name)
    typetracer_env$par_names <- methods::formalArgs (typetracer_env$fn)

    typetracer_env$classes <- vapply (typetracer_env$par_names, function (p) {

        res <- NULL

        if (p %in% ls (fn_env)) {
            res <- tryCatch (
                get (p, envir = fn_env, inherits = FALSE),
                error = function (e) NULL)
        }

        if (is.null (res)) {
            res <- tryCatch (
                eval (typetracer_env$pars [[p]], envir = fn_env),
                error = function (e) NULL)
        }

        c (class (res) [1],
           storage.mode (res),
           length (res))

    }, character (3))

    typetracer_env$classes <- data.frame (t (typetracer_env$classes))
    colnames (typetracer_env$classes) <- c ("class", "storage.mode", "length")
    typetracer_env$classes$fn_name <- as.character (typetracer_env$fn_name)
    typetracer_env$classes$p <- typetracer_env$par_names

    typetracer_env$cols <- c ("fn_name", "p", "class", "storage.mode", "length")
    typetracer_env$classes <- typetracer_env$classes [, typetracer_env$cols]

    apply (typetracer_env$classes, 1, function (i) {
                          writeLines (paste0 (i, collapse = ","),
                                      typetracer_env$typetracer_con)
            })
    close (typetracer_env$typetracer_con)

    rm (typetracer_env)
}
