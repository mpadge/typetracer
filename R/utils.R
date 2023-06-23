pkg_name_from_desc <- function (path) {

    desc <- list.files (
        path,
        pattern = "DESCRIPTION",
        recursive = TRUE,
        full.names = TRUE
    )

    if (length (desc) < 1L) {
        stop ("No 'DESCRIPTION' file found", call. = FALSE)
    } else if (length (desc) > 1L) {
        stop ("Multiple 'DESCRIPTION' files found", call. = FALSE)
    }

    as.character (read.dcf (desc) [, "Package"])
}

get_pkg_lib_path <- function (package, lib_paths) {

    pkg_path <- tryCatch (
        find.package (package, lib.loc = lib_paths),
        error = function (e) NULL
    )

    if (is.null (pkg_path)) {
        stop (
            "Package '", package, "' is not installed. Please ",
            "install locally, or use 'devtools::load_all()' ",
            "before calling 'trace_package()'",
            call. = FALSE
        )
    }

    lib_path <- normalizePath (file.path (pkg_path, ".."))

    return (lib_path)
}

set_trace_list_option <- function (trace_lists) {

    options (typetracer_trace_lists = trace_lists)
}

get_trace_lists_param <- function () {

    op <- options ("typetracer_trace_lists") [[1]]
    if (length (op) == 0L) {
        op <- FALSE
    }
    return (op)
}
