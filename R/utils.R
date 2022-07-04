reassign_function_body <- function (fun, body) {
    invisible (.Call (reassign_function_body_, fun, body))
}

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
