
get_typetrace_dir <- function () {

    td <- getOption ("typetracedir")
    if (is.null (td)) {
        td <- tempdir ()
    }
    return (td)
}

clear_fn_bodies_dir <- function () {

    fn_bodies_dir <- file.path (get_typetrace_dir (), "fn_bodies")
    if (dir.exists (fn_bodies_dir)) {
        has_files <- length (list.files (fn_bodies_dir)) > 0L
        if (has_files && interactive ()) {
            chk <- readline (paste0 (
                "All functions should first be uninjected before calling ",
                "this function. Do you wish to continue (y/n)? "
            ))
            if (tolower (substring (chk, 1, 1)) != "y") {
                stop (
                    "Please call 'uninject_tracer(<fn_name>) first",
                    call. = FALSE
                )
            }
        }
        unlink (fn_bodies_dir, recursive = TRUE)
    }
}

list_traces <- function () {

    td <- get_typetrace_dir ()
    list.files (
        td,
        full.names = TRUE,
        pattern = "^typetrace\\_.*\\.Rds$"
    )
}
