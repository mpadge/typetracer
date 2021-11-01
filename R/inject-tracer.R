
#' Inject parameter tracer into one function
#'
#' @param f Name of function as character string
#' @param e Environment in which function is defined; generally
#' `as.environment("package:<pkg_name>")`.
#' @return Nothing (will error on fail).
#'
#' @export
inject_tracer <- function (f, e) {

    get_types <- utils::getFromNamespace ("get_types", "typetracer")
    code <- body (get_types)
    invisible (inject_code (code, f))
}
