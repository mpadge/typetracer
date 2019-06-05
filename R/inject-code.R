#' Injects code into an existing function.
#'
#' The `code` argument is recorded without evaluation and it will be inserted
#' at the beginning of the `fun`. If `where` is not `onentry` it will be
#' wrapped in `on.exit` call. If `where` is `onerror`, the wrapped code will
#' further make the `code` to run only in the case the function failed with an
#' error condition.
#'
#' @param fun a function into which the code should be inserted.
#' @param code an expression that should be inserted.
#' @param where a indicates at which point of the `fun` invocation the given
#'     `code` shall run.
#'
#' @export
inject_code <- function(fun, code, where=c("onentry", "onexit", "onerror")) {
    stopifnot(is.function(fun))

    code <- substitute(code)
    wrapped_code <- wrap_code(code, where)
    new_body <- prepend_code(body(fun), wrapped_code)

    invisible(reassign_function_body(fun, new_body))
}

prepend_code <- function(orig_code, code) {
    if (identical(orig_code[[1]], as.name("{"))) {
        as.call(append(as.list(orig_code), code, 1))
    } else {
        call("{", code, orig_code)
    }
}

wrap_code <- function(code, where) {
    where <- match.arg(where, c("onentry", "onexit", "onerror"), several.ok=FALSE)

    switch(
        where,
        onentry=code,
        onexit=wrap_code_onexit(code),
        onerror=wrap_code_onerror(code)
    )
}

wrap_code_onexit <- function(code) {
    call("on.exit", code, TRUE, TRUE)
}

wrap_code_onerror <- function(code) {
    code <- substitute({
        default <- injectr:::.default_return_value
        retv <- returnValue(default=default)
        if (identical(default, retv)) CODE
    }, list(CODE=code))

    wrap_code_onexit(code)
}
