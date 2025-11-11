#' Code injected in function heads that gets the types of all parameters
#'
#' All variables and functions are defined within a new environment, to avoid
#' any confusion with variables or functions defined within functions in which
#' this code in injected, and to enable all of the local variables and functions
#' defined here to be easily deleted once types have been traced. This
#' environment also has to have an unambiguous and unique name.
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
        utils::getFromNamespace ("add_dotdotdot_params", "typetracer")
    typetracer_env <- typetracer_env$add_dotdotdot_params (typetracer_env)

    # 'get_str' is used in 'trace_one_param':
    typetracer_env$get_str <-
        utils::getFromNamespace ("get_param_str", "typetracer")
    typetracer_env$trace_one_param <-
        utils::getFromNamespace ("trace_one_param", "typetracer")
    typetracer_env$trace_one_list <-
        utils::getFromNamespace ("trace_one_list", "typetracer")
    typetracer_env$get_trace_lists_param <-
        utils::getFromNamespace ("get_trace_lists_param", "typetracer")

    typetracer_env$data <- lapply (typetracer_env$par_names, function (p) {
        dat_i <- typetracer_env$trace_one_param (typetracer_env, p, fn_env)
        trace_lists <- typetracer_env$get_trace_lists_param ()
        if (dat_i$typeof == "list" && trace_lists) {
            dat_i$list_data <-
                typetracer_env$trace_one_list (typetracer_env, p, fn_env)
        }
        return (dat_i)
    })

    typetracer_env$process_back_trace <-
        utils::getFromNamespace ("process_back_trace", "typetracer")
    # Initial trace has to be called in this environment:
    trace_dat <- rlang::trace_back (bottom = fn_env)

    # Uncomment this for debugging, and add "trace_dat" to "trace_objs" at start
    # of "load_traces" fn:
    # typetracer_env$data$trace_dat <- trace_dat

    typetracer_env$data$call_envs <-
        typetracer_env$process_back_trace (trace_dat, typetracer_env$fn_name)

    typetracer_env$data$fn_name <- as.character (typetracer_env$fn_name)
    typetracer_env$data$par_formals <- typetracer_env$par_formals
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

#' Recurse into one list-type parameter to extract internal structure.
#'
#' Standard evaluation only!
#' @noRd
trace_one_list <- function (typetracer_env, p, fn_env) {

    res <- tryCatch (
        get (p, envir = fn_env, inherits = FALSE),
        error = function (e) NULL
    )

    # non-standard evaluation, which is also necessary for lists passed as
    # `...`:
    if (is.null (res)) {
        res <- tryCatch (
            eval (typetracer_env$pars [[p]], envir = fn_env),
            error = function (e) NULL
        )
    }
    if (is.null (res)) {
        return (res)
    }

    list_str <- lapply (seq_along (res), function (i) {
        list (
            par = names (res) [i],
            class = class (res [[i]]),
            typeof = typeof (res [[i]]),
            storage_mode = storage.mode (res [[i]]),
            mode = mode (res [[i]]),
            length = length (res [[i]]),
            par_uneval = NA_character_,
            par_eval = NA_character_
        )
    })

    return (list_str)
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
process_back_trace <- function (trace_dat, fn_name) {

    call_envs <- data.frame (
        name = NA_character_,
        file = NA_character_,
        linestart = NA_integer_,
        lineend = NA_integer_,
        namespace = NA_character_
    )
    if (length (fn_name) == 0L) {
        return (call_envs [-1, ])
    }

    # Reduce to only calls at same level as the actual function, which then
    # includes any embedded environments of those, such as testthat expectations
    # or 'tryCatch' calls. Those will then be first on the call_env list in the
    # final reduction to one row, below.
    has_fn_name <- vapply (trace_dat$call, function (i) {
        index <- seq_along (i)
        p <- NULL
        while (is.null (p) && length (index) > 0L) {
            p <- tryCatch (
                parse (text = i [index], encoding = "UTF-8"),
                error = function (e) NULL
            )
            index <- index [-length (index)]
        }
        pd <- tryCatch (
            utils::getParseData (p),
            error = function (e) NULL
        )
        if (is.null (pd)) {
            return (FALSE)
        }
        index <- which (pd$token %in% c ("SYMBOL", "SYMBOL_FUNCTION_CALL"))
        fns <- pd$text [index]
        return (any (fns == fn_name))
    }, logical (1L))

    trace_dat <- trace_dat [which (has_fn_name), ]

    if (nrow (trace_dat) == 0L) {
        return (call_envs)
    }

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

    if (nrow (call_envs) > 0L) {
        # assume first branch of trace_back is desired env
        call_envs <- call_envs [1, ]
    }

    return (call_envs)
}
