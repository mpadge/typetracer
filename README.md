<!-- badges: start -->

[![R-CMD-check](https://github.com/mpadge/typetracer/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/typetracer/actions)
[![codecov](https://codecov.io/gh/mpadge/typetracer/branch/main/graph/badge.svg)](https://codecov.io/gh/mpadge/typetracer)
<!-- badges: end -->

# typetracer

Trace function parameter types in R packages.

    library (typetracer)

## Example

Define a function, and use `inject_tracer` to inject parameter tracers
used to trace parameter types on each call.

    f <- function (x, y, z, ...) {
        x * x + y * y
    }
    inject_tracer (f, .GlobalEnv)

Calls to the function, `f`, will then trace each parameter of the
function. The current demonstration-only version extracts values for
`storage.type` and `length`. These values can be accessed with the
`load_traces` function, with this example additionally demonstrating
that missing parameters (in this case, `z`) are not traced.

    val <- f (x = 1:2, y = 3:4 + 0.)
    load_traces ()

    ## # A tibble: 2 × 4
    ##   `function` parameter storage_mode length
    ##   <chr>      <chr>     <chr>         <int>
    ## 1 f          x         integer           2
    ## 2 f          y         double            2

The following example demonstrates that additional parameters passed via
`...` are successfully traced. Note also the `clear_traces()` function
which removes all previous traces, ensuring that `load_traces()` will
only return the most recently generated traces.

    clear_traces ()
    val <- f (x = 1:2, y = 3:4 + 0., a = "blah", b = list (a = 1, b = "b"))
    load_traces ()

    ## # A tibble: 4 × 4
    ##   `function` parameter storage_mode length
    ##   <chr>      <chr>     <chr>         <int>
    ## 1 f          x         integer           2
    ## 2 f          y         double            2
    ## 3 f          a         character         1
    ## 4 f          b         list              2
