
# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    options ("typetracedir" = tempdir ())

}

.onUnload <- function (libname, pkgname) { # nolint

    options ("typetracedir" = NULL)
    f <- file.path (tempdir (), "fn_bodies")
    if (dir.exists (f)) {
        unlink (f, recursive = TRUE)
    }
}
# nocov end
