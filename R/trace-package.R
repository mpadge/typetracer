
#' Trace all parameters for all functions in a specified package
#'
#' @param package Name of package to be traced (as character value)
#' @return A `data.frame` of data on every parameter of every function as
#' specified in code provided in package examples.
#' @export
trace_package <- function (package = NULL) {

    checkmate::assert_character (package)
    checkmate::assert_scalar (package)

    p <- paste0 ("package:", package)
    if (!p %in% search ()) {
        attachNamespace (package)
    }

    fns <- ls (p, all.names = TRUE)
    pkg_env <- as.environment (p)
    for (f in fns) {
        f <- get (f, envir = pkg_env)
        inject_tracer (f)
    }

    trace_package_exs (package)

    traces <- load_traces (quiet = TRUE)

    clear_traces ()

    for (f in fns) {
        f <- get (f, envir = pkg_env)
        uninject_tracer (f)
    }

    return (traces)
}

trace_package_exs <- function (package) {

    exs <- get_pkg_examples (package)

    # suppress any plot output
    dev <- options ()$"device"
    options (device = NULL)
    o <- suppressWarnings (
        out <- tryCatch (
            eval (parse (text = exs)),
            error = function (e) NULL)
    )
    options (device = dev)
}

get_pkg_examples <- function (package) {

    rd <- tools::Rd_db (package)

    has_exs <- vapply (rd, function (i) {
                out <- vapply (i, function (j)
                        any (attr (j, "Rd_tag") == "\\examples"),
                        logical (1))
                any (out)
    }, logical (1))

    exs <- lapply (rd [which (has_exs)], function (i) {
        f <- tempfile ()
        tools::Rd2ex (i, out = f)
        out <- brio::read_lines (f)
        file.remove (f)
        return (out)
    })

    exs <- unname (do.call (c, exs))

    return (exs)
}
