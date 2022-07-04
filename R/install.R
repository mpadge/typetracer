#' Pre-install package in temporary `libPath`
#'
#' @param path Local path to package source
#' @param quiet If `FALSE`, display progress information on screen.
#' @return Path to temporary location where package is installed from
#' @note Largely based on `covr` code.
#' @noRd
pre_install <- function (path, quiet = FALSE) {

    flag_types <- c (
        "CFLAGS",
        "CXXFLAGS",
        "CXX1XFLAGS",
        "CXX11FLAGS",
        "CXX14FLAGS",
        "CXX17FLAGS",
        "CXX20FLAGS"
    )
    flags <- "-O0" # No compiler optimsation; strict code correctness only
    flags <- rep (flags, length (flag_types))
    names (flags) <- flag_types

    install_path <- tempfile (pattern = "R_LIBS")
    dir.create (install_path)

    withr::with_makevars(flags, assignment = "+=",
        utils::install.packages(repos = NULL,
                                lib = install_path,
                                path,
                                type = "source",
                                INSTALL_opts = c("--example",
                                                 "--install-tests",
                                                 "--with-keep.source",
                                                 "--with-keep.parse.data",
                                                 "--no-staged-install",
                                                 "--no-multiarch"),
                                quiet = quiet)
    )

    return (install_path)
}
