# typetracer

Trace function parameter types in R packages.

    library (typetracer)

## Example

Define a function, and use [`injectr` code](https://github.com/PRL-PRG)
to inject the `get_types` function used to trace parameter types:

    f <- function (x, y) {
        x * x + y * y
    }
    code <- body (typetracer:::get_types)
    inject_code (code, f)

Calls to the function, `f`, will then dump traces to a file in the
temporary directory associated with the R process used to load the
`typetracer` package. This ensures that traces will still work even when
called in multi-threaded processes. The following code shows an example
call to the `f` function, and subsequent extraction of the traces which
are dumped to files prefixed with `typetrace_`. Current demo-only
version dumps `storage.type` and `length`.

    val <- f (x = 1:2, y = 3:4)
    traces <- list.files (tempdir (), pattern = "^typetrace\\_", full.names = TRUE)
    print (readLines (traces), width = 20)

    ## [1] "f,x,integer,2"
    ## [2] "f,y,integer,2"
