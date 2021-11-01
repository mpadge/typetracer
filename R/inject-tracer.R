
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

    get_types <- utils::getFromNamespace ("get_types", "typetracer")
    code <- body (get_types)
    invisible (inject_code (code, f))
}
