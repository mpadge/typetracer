
#' Inject parameter tracer into one function
#'
#' @param f A function (that is, an object of class "function", and not a
#' character string).
#' @param e Environment in which function is defined; generally
#' `as.environment("package:<pkg_name>")`.
#' @return Nothing (will error on fail).
#'
#' @note The tracer is defined in the internal `get_types` function. This uses
#' an `options` variable defined on package load for the current `tempdir`,
#' defining a single location where all traced values are dumped. This is done
#' via `options` to allow both multi-threaded function calls and calls via
#' \pkg{callr} to be traced.
#' @export
inject_tracer <- function (f, e) {

    checkmate::assert_function (f)
    checkmate::assert_environment (e)

    # save body for re-injection:
    f_name <- deparse (substitute (f))
    f_name <- cache_file_name (f, f_name, e)
    saveRDS (object = body (f), file = f_name)

    get_types <- utils::getFromNamespace ("get_types", "typetracer")
    code <- body (get_types)

    # check whether OpenCurlyBrace (`{`) has been redefined
    env <- environment (f)
    if (is.null (env)) {
        env <- as.environment ("package:base")
    }
    ocb_remapped <- !identical (get ("{", envir = env), .Primitive ("{"))

    fun_body <- body (f)

    new_body <- prepend_code (fun_body, code, ocb_remapped)

    invisible (reassign_function_body (f, new_body))
}

cache_file_name <- function (f, f_name, e) {

    cache_dir <- file.path (getOption ("typetracedir"),
                            "fn_bodies")
    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    file.path (cache_dir,
               paste0 ("typetrace--",
                       f_name,
                       "--",
                       environmentName (e),
                       ".Rds"))
}


#' Remove parameter tracer from one function
#'
#' This function removes traces previous injected into functions with the
#' \link{inject_tracer} function.
#'
#' @inheritParams inject_tracer
#' @export
uninject_tracer <- function (f, e) {

    checkmate::assert_function (f)
    checkmate::assert_environment (e)

    f_name <- deparse (substitute (f))
    f_name <- cache_file_name (f, f_name, e)
    if (!file.exists (f_name)) {
        return (FALSE)
    }

    body <- readRDS (f_name)
    reassign_function_body (f, body)

    return (TRUE)
}
