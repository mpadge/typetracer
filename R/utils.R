reassign_function_body <- function (fun, body) {
    invisible (.Call (reassign_function_body_, fun, body))
}
