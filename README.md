<!-- badges: start -->

[![R-CMD-check](https://github.com/mpadge/typetracer/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/typetracer/actions)
[![codecov](https://codecov.io/gh/mpadge/typetracer/branch/main/graph/badge.svg)](https://codecov.io/gh/mpadge/typetracer)
<!-- badges: end -->

# typetracer

Trace function parameter types in R packages.

    library (typetracer)

## Example #1

Define a function, and use `inject_tracer` to inject parameter tracers
used to trace parameter types on each call. The following function
includes an additional parameter, `z`, which is left undefined here,
along with `...` to allow passing of arbitrary parameter values.

    f <- function (x, y, z, ...) {
        x * x + y * y
    }
    inject_tracer (f, .GlobalEnv)

Calls to the function, `f`, will then trace each parameter of the
function. The current demonstration-only version extracts values for
`storage.type` and `length`. These values can be accessed with the
`load_traces` function, with this example additionally demonstrating
that the additional parameter, `z`, is not traced when left undefined.

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

## Example #2

This section presents a more complex example tracing parameters for a
selection of functions from the base R package.

### Select functions and extract example code

Start with a selection of functions:

    pkg <- "base"
    fns <- ls (paste0 ("package:", pkg), all.names = TRUE)
    pkg_env <- as.environment (paste0 ("package:", pkg))
    is_prim <- vapply (fns, function (i) is.primitive (get (i, envir = pkg_env)),
                       logical (1L))
    fns <- fns [which (!is_prim)]
    # Then reduce to a small sample of those:
    fns <- grep ("^[[:alpha:]]", fns, value = TRUE) [1:50]

Then extract the example code for those functions from the `.Rd`
database of the base package. For extra simplicity, the functions are
reduced to only those with matching `.Rd` names.

    rd <- tools::Rd_db (pkg)
    fns <- fns [which (fns %in% gsub ("\\.Rd$", "", names (rd)))]
    rd <- rd [which (gsub ("\\.Rd$", "", names (rd)) %in% fns)]

The following lines extract the example code from those `.Rd` entries:

    exs <- lapply (rd, function (i) {
        f <- tempfile ()
        tools::Rd2ex (i, out = f)
        if (!file.exists (f)) { # no examples
            return (NULL)
        }
        
        out <- brio::read_lines (f)
        file.remove (f)
        return (out)
    })
    exs <- unname (do.call (c, exs))

### Inject type tracers in all of those functions

The `system.time` call at the start of the following code demonstrations
that injection is almost instantaneous.

    system.time ({
        for (f in fns) {
            f <- get (f, envir = pkg_env)
            inject_tracer (f, pkg_env)
        }
    })

    ##    user  system elapsed 
    ##   0.032   0.006   0.039

### Run the example code

Examples can be run through a simple `eval` call, but this must
generally be wrapped like in the following example to suppress
generation of plots and warning messages.

    pdf (file = NULL)
    o <- suppressWarnings (
        res <- eval (parse (text = exs))
        )

    ## function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, 
    ##     pattern, sorted = TRUE)  
    ## <simpleError in args("graphics::plot.default"): could not find function "graphics::plot.default">

    chk <- dev.off ()

### Load the traces

Finally, load the resultant type traces as above

    load_traces ()

    ## # A tibble: 2,792 × 4
    ##    `function` parameter storage_mode length
    ##    <chr>      <chr>     <chr>         <int>
    ##  1 append     x         list              0
    ##  2 append     values    language          3
    ##  3 append     x         list             16
    ##  4 append     values    language          3
    ##  5 append     x         list              0
    ##  6 append     values    language          3
    ##  7 append     x         list              8
    ##  8 append     values    language          1
    ##  9 append     x         list              4
    ## 10 append     values    language          4
    ## # … with 2,782 more rows
