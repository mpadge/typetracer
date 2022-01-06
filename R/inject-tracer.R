
#' Inject parameter tracer into one function
#'
#' @param f Name of function as character string
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

    checkmate::assert_character (f)
    checkmate::assert_scalar (f)
    checkmate::assert_environment (e)

    cache_body (f, e)

    get_types <- utils::getFromNamespace ("get_types", "typetracer")
    code <- body (get_types)
    invisible (inject_code (code, get (f, envir = e)))
}

cache_body <- function (f, e) {

    cache_dir <- file.path (getOption ("typetracedir"),
                            "fn_bodies")
    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    cache_file <- file.path (cache_dir,
                             paste0 ("typetrace--",
                                     f,
                                     "--",
                                     environmentName (e),
                                     ".Rds"))

    fn_body <- body (get (f, envir = e))
    saveRDS (object = fn_body, file = cache_file)
}
