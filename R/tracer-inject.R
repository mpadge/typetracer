
#' Inject parameter tracer into one function
#'
#' @param f A function (that is, an object of class "function", and not a
#' character string).
#' @inheritParams trace_package
#' @return Nothing (will error on fail).
#'
#' @note The tracer is defined in the internal `typetracer_header()` function.
#' This uses an `options` variable defined on package load for the current
#' `tempdir`, defining a single location where all traced values are dumped.
#' This is done via `options` to allow both multi-threaded function calls and
#' calls via \pkg{callr} to be traced.
#' @export
#' @examples
#' f <- function (x, y, z, ...) {
#'     x * x + y * y
#' }
#' inject_tracer (f)
#' val <- f (1:2, 3:4 + 0., a = "blah")
#' x <- load_traces ()
#'
#' # Traces should always be "uninjected":
#' uninject_tracer (f)
#' # Traces may also be removed:
#' clear_traces ()
inject_tracer <- function (f, trace_lists = FALSE) {

    checkmate::assert_function (f)
    set_trace_list_option (trace_lists)

    # save body for re-injection:
    f_name <- deparse (substitute (f))
    f_name <- cache_file_name (f, f_name)
    saveRDS (object = body (f), file = f_name)

    typetracer_header <-
        utils::getFromNamespace ("typetracer_header", "typetracer")
    code <- body (typetracer_header)

    fun_body <- body (f)

    new_body <- prepend_code (fun_body, code)

    invisible (reassign_function_body (f, new_body))
}

cache_file_name <- function (f, f_name) {

    cache_dir <- file.path (get_typetrace_dir (), "fn_bodies")
    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    file.path (
        cache_dir,
        paste0 (
            "typetrace--",
            f_name,
            ".Rds"
        )
    )
}

reassign_function_body <- function (fun, body) {
    invisible (.Call (reassign_function_body_, fun, body))
}


#' Remove parameter tracer from one function
#'
#' This function removes traces previous injected into functions with the
#' \link{inject_tracer} function.
#'
#' @inheritParams inject_tracer
#' @return Logical value indicating whether or not tracer was able to be removed
#' ("uninjected").
#' @export
#' @examples
#' f <- function (x, y, z, ...) {
#'     x * x + y * y
#' }
#' inject_tracer (f)
#' val <- f (1:2, 3:4 + 0., a = "blah")
#' x <- load_traces ()
#'
#' # Traces should always be "uninjected":
#' uninject_tracer (f)
#' # Traces may also be removed:
#' clear_traces ()
uninject_tracer <- function (f) {

    checkmate::assert_function (f)

    f_name <- deparse (substitute (f))
    f_name <- cache_file_name (f, f_name)
    if (!file.exists (f_name)) {
        return (FALSE)
    }

    body <- readRDS (f_name)
    reassign_function_body (f, body)
    file.remove (f_name)
}
