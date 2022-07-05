#' Pre-install package in temporary `libPath`
#'
#' @param path Local path to package source.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return Path to temporary location where package is installed from.
#' @note The code inside `is.null(path)` is largely based on 'covr' code,
#' from the `covr.R` file in that package. This original code is distributed
#' under MIT License, with copyright held by 'covr authors'
#' @noRd
pre_install <- function (package, path = NULL, quiet = FALSE) {

    p <- paste0 ("package:", package)
    pkg_attached <- p %in% search ()
    if (pkg_attached) {
        unloadNamespace (package)
        pkg_attached <- p %in% search () # FALSE
    }

    libs <- .libPaths ()

    if (!is.null (path)) {

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

        libs <- c (install_path, libs)
    }

    if (!pkg_attached) { # always

        lib_path <- get_pkg_lib_path (package, libs)
        loadNamespace (package, lib.loc = lib_path, keep.source = TRUE)
        attachNamespace (package)
    }

    lib_path <- get_pkg_lib_path (package, libs)

    return (lib_path)
}

#' Reload package from default library location
#'
#' @param pkg_name Name of package to be re-loaded
#' @param lib_path Path to temporary library location from which package was
#' installed.
#'
#' @note This is directly modified from covr:::run_commands. Here, it just
#' re-loads the package from the default library location, ensuring that the
#' modified version is removed.
#'
#' @noRd
reload_pkg <- function (pkg_name, lib_path) {

    # If package was not initially installed, don't do anything:
    install_path <- tryCatch (
        find.package (package, lib.loc = .libPaths ()),
        error = function (e) NULL
    )
    if (is.null (install_path)) {
        return (FALSE)
    }

    infile <- file.path (lib_path, paste0 (pkg_name, "-reload.Rout"))
    outfile <- file.path (lib_path, paste0 (pkg_name, "-reload-out.Rout"))
    cat(
        "library ('", pkg_name, "')\n",
        file = infile, sep = "")
    cmd <- paste (shQuote (file.path (R.home ("bin"), "R")),
                 "CMD BATCH --vanilla --no-timing",
                 shQuote (infile), shQuote (outfile))
    res <- system (cmd)
    if (res != 0L) {
        stop ("Command failed", call. = FALSE)
    }

    return (res == 0L)
}
