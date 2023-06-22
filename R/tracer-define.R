
#' Code injected in function heads that gets the types of all parameters
#'
#' All variables are defined within a new environment, to avoid any confusion
#' with variables defined within functions in which this code in injected, and
#' to enable all of the local variables defined here to be easily deleted once
#' types have been traced. This environment also has to have an unambiguous and
#' unique name.
#' @noRd
typetracer_header <- function () {

    typetracer_env <- new.env (parent = emptyenv ())

    # temp file to dump trace:
    typetracer_env$td <- options ("typetracedir")
    typetracer_env$nm <- paste0 (sample (c (letters, LETTERS), 8),
        collapse = ""
    )
    typetracer_env$fname <- file.path (
        typetracer_env$td,
        paste0 ("typetrace_", typetracer_env$nm, ".Rds")
    )

    typetracer_env$trace_dir <- options ("typetracedir")$typetracedir
    typetracer_env$num_traces <- length (list.files (
        typetracer_env$trace_dir,
        pattern = "^typetrace\\_"
    ))

    # Extract values. `match.call` returns the *expressions* submitted to the
    # call, while the evaluated versions of formalArgs are stored in the
    # environment. `get` is used for the latter to avoid re-`eval`-ing, but
    # `...` args are not eval'd on function entry.
    typetracer_env$fn_call <- match.call (expand.dots = TRUE)
    typetracer_env$fn_name <- typetracer_env$fn_call [[1]]
    typetracer_env$pars <- as.list (typetracer_env$fn_call [-1L])

    fn_env <- environment ()

    typetracer_env$fn <- match.fun (typetracer_env$fn_name)
    typetracer_env$par_names <- methods::formalArgs (typetracer_env$fn)
    typetracer_env$par_formals <- formals (typetracer_env$fn)

    # Bring in and run typetracer internal functions:
    typetracer_env$add_dotdotdot_params <-
        getFromNamespace ("add_dotdotdot_params", "typetracer")
    typetracer_env <- typetracer_env$add_dotdotdot_params (typetracer_env)

    # 'get_str' is used in 'trace_one_param':
    typetracer_env$get_str <- getFromNamespace ("get_param_str", "typetracer")
    typetracer_env$trace_one_param <-
        getFromNamespace ("trace_one_param", "typetracer")
    typetracer_env$data <- lapply (typetracer_env$par_names, function (p) {
        typetracer_env$trace_one_param (typetracer_env, p, fn_env)
    })

    typetracer_env$process_back_trace <-
        getFromNamespace ("process_back_trace", "typetracer")
    # Initial trace has to be called in this environment:
    trace_dat <- rlang::trace_back (bottom = fn_env)
    typetracer_env$data$call_envs <-
        typetracer_env$process_back_trace (trace_dat)

    typetracer_env$data$fn_name <- as.character (typetracer_env$fn_name)
    typetracer_env$data$formals <- typetracer_env$par_formals
    typetracer_env$data$num_traces <- typetracer_env$num_traces

    saveRDS (typetracer_env$data, typetracer_env$fname)

    rm (typetracer_env)
}

#' Add information on any additional parameters passed via '...'
#' @noRd
add_dotdotdot_params <- function (typetracer_env) {

    if ("..." %in% typetracer_env$par_names) {

        typetracer_env$dot_names <- names (typetracer_env$fn_call)

        index <- which (nzchar (typetracer_env$dot_names) &
            !typetracer_env$dot_names %in% typetracer_env$par_names)
        typetracer_env$dot_names <- typetracer_env$dot_names [index]

        typetracer_env$par_names <- c (
            typetracer_env$par_names,
            typetracer_env$dot_names
        )
    }

    return (typetracer_env)
}

#' Return structure of parameters as character strings
#'
#' See https://rpubs.com/maechler/R_language_objs
#' @noRd
get_param_str <- function (x, max.length = 1000L) { # nolint

    r <- tryCatch (format (x), error = function (e) e)
    r <- if (inherits (r, "error")) {
        tryCatch (as.character (x), error = function (e) e)
    } else {
        paste (r, collapse = " ")
    }
    r <- if (inherits (r, "error")) {
        tryCatch (utils::capture.output (x), error = function (e) e)
    } else {
        paste (r, collapse = " ")
    }
    substr (r, 1L, max.length)
}

#' Extract information on one parameter
#' @noRd
trace_one_param <- function (typetracer_env, p, fn_env) {

    res <- NULL

    # standard evalation for named parameters which exist in fn_env:
    if (p %in% ls (fn_env)) {
        res <- tryCatch (
            get (p, envir = fn_env, inherits = FALSE),
            error = function (e) NULL
        )
    }

    # non-standard evaluation:
    if (is.null (res)) {
        res <- tryCatch (
            eval (typetracer_env$pars [[p]], envir = fn_env),
            error = function (e) NULL
        )
    }

    s <- "NULL"
    if (!is.null (res)) {
        s <- typetracer_env$get_str (typetracer_env$pars [[p]])
        if (length (s) > 1L) {
            s <- paste0 (s, collapse = "; ")
        }
        if (is.null (s)) {
            s <- "NULL"
        }
    }

    list (
        par = p,
        class = class (res),
        typeof = typeof (res),
        storage_mode = storage.mode (res),
        mode = mode (res),
        length = length (res),
        par_uneval = s,
        par_eval = res
    )
}

#' Extract environments of function calls
#'
#' Note that rlang enumerates envs from "0" for the calling environment. For
#' srcref structure, see:
#' https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Murdoch.pdf
#' Note that line numbers in srcref are from parsed versions, so will generally
#' not exactly match.
#'
#' @param trace_dat A back-traced syntax tree returned from
#' 'rlang::trace_back()'.
#' @noRd
process_back_trace <- function (trace_dat) {

    trace_dat <- trace_dat [which (trace_dat$parent == 0), ]

    call_envs <- lapply (trace_dat$call, function (i) {
        call_i <- data.frame (
            name = as.character (as.name (as.list (i) [[1]])),
            file = NA_character_,
            linestart = NA_integer_,
            lineend = NA_integer_
        )
        if (!is.null (attributes (i)$srcref)) {
            call_i$file <- attr (attributes (i)$srcref, "srcfile")$filename
            call_i$linestart <- attr (i, "srcref") [1]
            call_i$lineend <- attr (i, "srcref") [3]
        }
        return (call_i)
    })
    call_envs <- do.call (rbind, call_envs)
    call_envs$namespace <- trace_dat$namespace
    index <- which (is.na (call_envs$namespaces))
    if (length (index) > 0L) {
        call_envs$namespace [index] <- trace_dat$scope [index]
    }
    call_envs <- call_envs [which (call_envs$namespace != "typetracer"), ]
    if (nrow (call_envs) > 0L) {
        # assume first branch of trace_back is desired env
        call_envs <- call_envs [1, ]
    }

    return (call_envs)
}
