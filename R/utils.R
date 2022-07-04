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

get_pkg_lib_path <- function (package, libs) {

    lib_path <- tryCatch (
                          find.package (package, lib.loc = libs),
                          error = function (e) NULL
    )

    if (is.null (lib_path)) {
        stop (
              "Package '", package, "' is not installed. Please ",
              "install locally, or use 'devtools::load_all()' ",
              "before calling 'trace_package()'",
              call. = FALSE
        )
    }

    return (gsub (paste0 (.Platform$file.sep, package), "", lib_path))
}
