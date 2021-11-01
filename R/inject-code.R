
# Modified from 'injectr' by Filip Krikava
# https://github.com/PRL-PRG/injectr

#' Injects code into an existing function.
#'
#' The `code` argument is recorded without evaluation and it will be inserted
#' at the beginning of the `fun`. If it is a symbol, its value will be looked
#' starting in the callee frame. If `where` is not `onentry` it will be wrapped
#' in `on.exit` call. If `where` is `onerror`, the wrapped code will further
#' make the `code` to run only in the case the function failed with an error
#' condition.
#'
#' This function modifies the original function.
#'
#' @param code the code that should be inserted.
#' @param fun the function into which the code should be inserted.
#' @param where indicates at which point of the `fun` invocation the given
#'     `code` shall run. It can be one of the following: `onentry`, `onexit`,
#'     `onsuccess`, `onerror`.
#' @param wrap indicates if it the given `code` should be wrapped in an
#'     anonymous function (`TRUE`) or inserted as is (`FALSE`).
#' @return NULL invisibly
#'
#' @export
inject_code <- function (code, fun, where = "onentry", wrap = FALSE) {
    code <- substitute (code)

    # this is a dispatch based on SEXP type rather than on the class attribute
    # the reason why we do not use S3 dispatch based on class is that there are
    # too many classes for calls (cf. lang2str in R/src/main/attrib.c)
    switch (
        typeof (code),
        symbol = inject_symbol (code, fun, where, wrap),
        language = inject_language (code, fun, where, wrap),
        default = stop ("Unsupported code type: ", typeof (code))
    )
}

inject_symbol <- function (name, fun, where, wrap) {
    stopifnot (is.symbol (name))

    tryCatch ({
        sym <- resolve_name (name, sys.nframe () - 1)
    }, error = function (e) {
        stop ("Unable to resolve symbol `", name, "'")
    })

    switch (
        typeof (sym),
        closure = inject_closure (sym, fun, where, wrap),
        expression = inject_expression (sym, fun, where, wrap),
        language = inject_language (sym, fun, where, wrap),
        stop ("Unsupported code type: ", typeof (sym))
    )
}

resolve_name <- function (name, pos) {
    v <- name
    while (pos >= 0 && is.symbol (v)) {
        tryCatch ({
            v <- get (as.character (v),
                      envir = sys.frame (pos),
                      inherits = FALSE)
        }, error = function (e) {
            if (pos == 0) stop (e)
        })
        pos <- pos - 1
    }
    v
}

inject_expression <- function (code, fun, where, wrap) {
    stopifnot (is.expression (code))
    stopifnot (is.function (fun))

    code <- if (wrap) {
        as.call (c (as.function (as.list (code))))
    } else {
        # get the content of the EXPRSXP
        #code[[1]]
        code
    }

    # checks if OpenCurlyBrace (i.e. `{`) has been redefined
    ocb_remapped <- !identical (get ("{", envir = environment (fun)),
                                .Primitive ("{"))

    wrapped_code <- prepare_code_to_run (code, where)

    fun_body <- body (fun)

    if (where != "onentry") {
        fun_body <- process_on_exit (fun_body)
    }

    new_body <- prepend_code (fun_body, wrapped_code, ocb_remapped)

    invisible (reassign_function_body (fun, new_body))
}

inject_language <- function (code, fun, where, wrap) {
    stopifnot (typeof (code) == "language")
    inject_expression (as.expression (code), fun, where, wrap)
}

inject_closure <- function (code, fun, where, wrap) {
    stopifnot (is.function (code))
    stopifnot (is.function (fun))

    if (wrap) {
        warning ("ignoring wrap = TRUE in inject_closure")
    }

    code <- as.call (c (code, lapply (names (formals (fun)), as.name)))

    inject_language (code, fun, where, FALSE)
}

prepend_code <- function (orig_code, code, use_primitive) {
    # is.language will not work since SYMSXP and EXPRSXP are also of language
    # type
    if (typeof (orig_code) == "language" &&
            identical (orig_code[[1]], as.name ("{")) &&
            !use_primitive) {
        as.call (append (as.list (orig_code), code, 1))
    } else if (use_primitive) {
        substitute (.Primitive ("{") (CODE, ORIG_CODE),
                    list (CODE = code, ORIG_CODE = orig_code))
    } else {
        substitute ({ CODE; ORIG_CODE },                        # nolint
                    list (CODE = code, ORIG_CODE = orig_code))
    }
}

prepare_code_to_run <- function (code, where) {
    where <- match.arg (where,
                        c ("onentry", "onexit", "onerror", "onsuccess"),
                        several.ok = FALSE)

    switch (
        where,
        onentry = code,
        onexit = wrap_code_onexit (code),
        onerror = wrap_code_onexit_state (code, failure = TRUE),
        onsuccess = wrap_code_onexit_state (code, failure = FALSE)
    )
}

wrap_code_onexit <- function (code) {
    call ("on.exit", code, TRUE, TRUE)
}

wrap_code_onexit_state <- function (code, failure) {
    code <- substitute ({
        #default <- typetracer:::.default_return_value
        default <- utils::getFromNamespace (".default_return_value",
                                            "typetracer")
        retv <- returnValue (default = default)
        if (identical (default, retv) == HAS_FAILED) CODE
    }, list (CODE = code, HAS_FAILED = failure))

    wrap_code_onexit (code)
}
