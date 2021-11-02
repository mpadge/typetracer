reassign_function_body <- function(fun, body) {
    invisible(.Call(reassign_function_body_, fun, body))
}

## TODO: Fix implementation to properly handle the complete call signature of
## on.exit
process_on_exit <- function(expr) {
    if (typeof (expr) == "language") {
        if (expr [[1]] == "on.exit") {
            expr [[3]] <- TRUE
            expr [[4]] <- FALSE
        }
        else {
            l <- length (expr)
            for (i in 1:l) {
                ## NOTE: expr[[i]] is used inplace to avoid missingness
                ##       errors in expressions of the form x[,i]
                if (typeof (expr [[i]]) == "language") {
                    expr [[i]] <- process_on_exit (expr[[i]])
                }
            }
        }
    }
    expr
}
