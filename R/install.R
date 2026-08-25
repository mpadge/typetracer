#' Session-level cache of already pre-installed packages.
#'
#' `pre_install()` used to reinstall + reload the target package from
#' scratch on every call, even when called repeatedly for the same package
#' within a single session (e.g. 'autotest' calling `trace_package()` more
#' than once against the same package). Each such cycle re-runs
#' `install.packages()` and unloads/reloads the package namespace, which is
#' expensive on its own and, worse, compounds across repeated calls: R does
#' not always release lazy-load database connections and namespace/method
#' registry state cleanly on `unloadNamespace()`, so repeated cycles lead to
#' growing memory use and increasingly slow `lazyLoadDBfetch` calls. Caching
#' the resulting `lib_path` per package lets subsequent calls skip the
#' reinstall and reuse the same temporary library, only re-attaching the
#' namespace if it isn't currently attached.
#'
#' This is deliberately restricted to installed packages traced by name
#' (`path` left `NULL`, i.e. resolved via `find.package()`), not to local
#' source directories: local packages are typically only traced once or
#' twice per session anyway, and reusing a cached reload for a package whose
#' local source may have just changed underfoot risks stale/corrupt reads
#' (e.g. of its help/Rd database) rather than the reinstall churn this cache
#' exists to avoid.
#' @noRd
typetracer_pkg_cache <- new.env (parent = emptyenv ())

#' Pre-install package in temporary `libPath`
#'
#' @param path Local path to package source.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return Path to temporary location where package is installed from.
#' @note Some of this code is slightly adapted from 'covr' code, from the
#' `covr.R` file in that package. This original code is distributed under MIT
#' License, with copyright held by 'covr authors'
#' @noRd
pre_install <- function (package, path = NULL, quiet = FALSE) {

    libs <- .libPaths ()

    is_installed_pkg <- is.null (path)

    if (is.null (path)) {
        # installed packages without local source. If packages are not
        # installed, `find.package()` errors with,
        # "there is no package called '...'".
        path <- find.package (package, lib.loc = libs)
    }

    p <- paste0 ("package:", package)

    if (is_installed_pkg) {
        cache_key <- paste0 (package, "::", path)
        cached_lib_path <- mget (
            cache_key,
            envir = typetracer_pkg_cache,
            ifnotfound = list (NULL)
        ) [[1]]
        if (!is.null (cached_lib_path) &&
            dir.exists (file.path (cached_lib_path, package))) {

            if (!p %in% search ()) {
                tryCatch (unloadNamespace (package), error = function (e) NULL)
                loadNamespace (package, lib.loc = cached_lib_path, keep.source = TRUE)
                attachNamespace (package)
            }

            return (cached_lib_path)
        }
    }

    pkg_attached <- p %in% search ()
    if (pkg_attached) {
        tryCatch (
            unloadNamespace (package),
            error = function (e) NULL
        )
    }


    # ----- BEGIN covr code

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

    withr::with_makevars (flags,
        assignment = "+=",
        utils::install.packages (
            repos = NULL,
            lib = install_path,
            path,
            type = "source",
            INSTALL_opts = c (
                "--example",
                "--install-tests",
                "--with-keep.source",
                "--with-keep.parse.data",
                "--no-staged-install",
                "--no-multiarch"
            ),
            quiet = quiet
        )
    )

    # ----- END covr code


    lib_path <- get_pkg_lib_path (package, install_path)
    if (!lib_path %in% libs) {
        libs <- c (lib_path, libs)
    }

    loadNamespace (package, lib.loc = lib_path, keep.source = TRUE)
    attachNamespace (package)

    if (is_installed_pkg) {
        assign (cache_key, lib_path, envir = typetracer_pkg_cache)
    }

    return (lib_path)
}

insert_counters_in_tests <- function (pkg_dir) {

    test_path <- file.path (pkg_dir, "tests", "testthat")
    if (!dir.exists (test_path)) {
        return (NULL)
    }

    test_files <- list.files (
        test_path,
        pattern = "^test",
        recursive = TRUE,
        full.names = TRUE
    )

    trace_dir <- options ("typetracedir")$typetracedir

    for (f in test_files) {

        p <- parse (f, keep.source = TRUE)
        p_injected <- lapply (seq_along (p), function (i) {
            pp_i <- parse (text = deparse (p [[i]]), keep.source = TRUE)
            pd_i <- utils::getParseData (pp_i, includeText = TRUE)
            testthat_start <- which (
                pd_i$token == "SYMBOL_FUNCTION_CALL" &
                    pd_i$text == "test_that"
            )
            if (length (testthat_start) == 0L) {
                return (deparse (p [[i]]))
            }
            str_const_i <- which (pd_i$token == "STR_CONST")
            str_const_i <-
                str_const_i [which (str_const_i > testthat_start) [1]]
            test_name <- gsub ("\\\"|\\\'", "", pd_i$text [str_const_i])

            # Locate the `{` via its parse-token position rather than
            # grep()-ing description text, which breaks on parens/braces/quotes.
            brace_i <- which (pd_i$token == "'{'")
            brace_i <- brace_i [which (brace_i > str_const_i) [1]]
            if (is.na (brace_i)) {
                return (deparse (p [[i]]))
            }
            index <- pd_i$line1 [brace_i]

            pd_i <- deparse (p [[i]])
            test_name <- gsub ("\\s+", "_", test_name)
            pd_i <- c (
                pd_i [seq (index)],
                "",
                paste0 (
                    "traces <- list.files (\"",
                    trace_dir,
                    "\", pattern = \"^typetrace_\", full.names = TRUE)"
                ),
                "ntraces <- length (traces)",
                paste0 (
                    "ftmp <- file.path (\"",
                    pkg_dir,
                    "\", \"tracetest_",
                    test_name,
                    ".txt\")"
                ),
                "writeLines (as.character (ntraces), ftmp)",
                "",
                pd_i [-seq (index)]
            )

            return (pd_i)
        })
        writeLines (unlist (p_injected), f)
    }
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
        find.package (pkg_name, lib.loc = .libPaths ()),
        error = function (e) NULL
    )
    if (is.null (install_path)) {
        return (FALSE)
    }

    fpath <- ifelse (
        grepl (tempdir (), lib_path),
        lib_path,
        tempdir ()
    )
    infile <- file.path (fpath, paste0 (pkg_name, "-reload.Rout"))
    outfile <- file.path (fpath, paste0 (pkg_name, "-reload-out.Rout"))
    cat (
        "library ('", pkg_name, "')\n",
        file = infile, sep = ""
    )
    cmd <- paste (
        shQuote (file.path (R.home ("bin"), "R")),
        "CMD BATCH --vanilla --no-timing",
        shQuote (infile), shQuote (outfile)
    )
    res <- system (cmd)
    if (res != 0L) {
        stop ("Command failed", call. = FALSE)
    }

    if (!identical (fpath, tempdir ())) {
        tryCatch (
            unlink (fpath, recursive = TRUE),
            error = function (e) NULL
        )
    }

    return (res == 0L)
}
