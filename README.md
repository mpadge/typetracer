# typetracer

Trace function parameter types in R packages.

    library (typetracer)

## Example

Define a function, and use `inject_tracer` to inject parameter tracers
used to trace parameter types on each call.

    f <- function (x, y) {
        x * x + y * y
    }
    inject_tracer (f, .GlobalEnv)

Calls to the function, `f`, will then dump traces to a file in the
temporary directory associated with the R process used to load the
`typetracer` package. This ensures that traces will still work even when
called in multi-threaded processes. The following code shows an example
call to the `f` function, and subsequent extraction of the traces.
Current demo-only version only traces `storage.type` and `length`.

    val <- f (x = 1:2, y = 3:4 + 0.)
    load_traces ()

    ## # A tibble: 2 × 4
    ##   `function` parameter storage_mode length
    ##   <chr>      <chr>     <chr>         <int>
    ## 1 f          x         integer           2
    ## 2 f          y         double            2
