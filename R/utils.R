reassign_function_body <- function(fun, body) {
    invisible(.Call(reassign_function_body_, fun, body))
}

create_duplicate <- function(x) {
    .Call(create_duplicate_, x)
}

sexp_address <- function(x) {
    .Call(sexp_address_, x)
}
