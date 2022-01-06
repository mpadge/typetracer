
# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    options ("typetracedir" = tempdir ())

}

.onUnload <- function (libname, pkgname) { # nolint

    options ("typetracedir" = NULL)
}
# nocov end
