
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
        unlink (fn_bodies_dir, recursive = TRUE)
    }
}
