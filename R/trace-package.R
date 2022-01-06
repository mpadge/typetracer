
#' Trace all parameters for all functions in a specified package
#'
#' @param Package Name of package to be traced (as character value)
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

    exs <- get_pkg_examples (package)

    pdf (file = NULL) # suppress any plot output
    o <- suppressWarnings (
        out <- eval (parse (text = exs))
    )
    chk <- dev.off ()

    traces <- load_traces ()

    clear_traces ()

    for (f in fns) {
        f <- get (f, envir = pkg_env)
        uninject_tracer (f)
    }

    return (traces)
}

get_pkg_examples <- function (package) {

    rd <- tools::Rd_db (package)

    exs <- lapply (rd, function (i) {
        f <- tempfile ()
        tools::Rd2ex (i, out = f)
        out <- brio::read_lines (f)
        file.remove (f)
        return (out)
    })

    exs <- unname (do.call (c, exs))

    return (exs)
}
