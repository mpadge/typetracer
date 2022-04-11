<!-- badges: start -->

[![R-CMD-check](https://github.com/mpadge/typetracer/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/typetracer/actions)
[![codecov](https://codecov.io/gh/mpadge/typetracer/branch/main/graph/badge.svg)](https://codecov.io/gh/mpadge/typetracer)
<!-- badges: end -->

# typetracer

Trace function parameter types in R packages.

    library (typetracer)

## Example \#1

Define a function, and use `inject_tracer` to inject parameter tracers
used to trace parameter types on each call. The following function
includes an additional parameter, `z`, which is left undefined here,
along with `...` to allow passing of arbitrary parameter values.

    f <- function (x, y, z, ...) {
        x * x + y * y
    }
    inject_tracer (f)

Calls to the function, `f`, will then trace each parameter of the
function. The current demonstration-only version extracts values for
`storage.type` and `length`. These values can be accessed with the
`load_traces` function, with this example additionally demonstrating:

1.  That the additional parameter, `z`, is not traced when left
    undefined; and
2.  That additional parameters passed via `...` are successfully traced.

<!-- -->

    val <- f (x = 1:2, y = 3:4 + 0., a = "blah", b = list (a = 1, b = "b"))
    load_traces ()

    ## # A tibble: 4 × 7
    ##   fn_name par_name class   storage_mode length par_uneval value
    ##   <chr>   <chr>    <chr>   <chr>         <int> <chr>      <chr>
    ## 1 f       x        integer integer           2 1:2        1 2  
    ## 2 f       y        numeric double            2 3:4 + 0    3 4  
    ## 3 f       z        NULL    NULL              0 NULL       NULL 
    ## 4 f       ...      NULL    NULL              0 NULL       NULL

Traces themselves are saved in the temporary directory of the current R
session, and the `load_traces()` function simple loads all traces
created in that session. The function `clear_traces()` removes all
traces, so that `load_traces()` will only load new traces produced after
that time.

## Example \#2

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
    fns <- grep ("^[[:alpha:]]", fns, value = TRUE) [1:30]

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
            inject_tracer (f)
        }
    })

    ##    user  system elapsed 
    ##   0.004   0.001   0.004

### Run the example code

Examples can be run through a simple `eval` call, but this must
generally be wrapped like in the following example to suppress
generation of plots and warning messages.

    pdf (file = NULL)
    o <- suppressWarnings (
        res <- eval (parse (text = exs))
        )
    chk <- dev.off ()

### Load the traces

Finally, load the resultant type traces as above

    load_traces ()

    ## # A tibble: 1,833 × 7
    ##    fn_name    par_name  class       storage_mode length par_uneval   value      
    ##    <chr>      <chr>     <chr>       <chr>         <int> <chr>        <chr>      
    ##  1 all.equal  target    character   character         8 target[[i]]  "holm     …
    ##  2 all.equal  current   CallRoutine list              4 current[[i]] "pnbinom <…
    ##  3 all.equal  ...       NULL        NULL              0 NULL         "NULL"     
    ##  4 all.equal  target    numeric     double          100 ..1          " 0.1  0.2…
    ##  5 all.equal  current   numeric     double          100 ..2          " 0.100000…
    ##  6 all.equal  ...       NULL        NULL              0 NULL         "NULL"     
    ##  7 all.equal  target    function    function          1 target[[i]]  "function …
    ##  8 all.equal  current   CallRoutine list              4 current[[i]] "pgamma <p…
    ##  9 all.equal  ...       NULL        NULL              0 NULL         "NULL"     
    ## 10 abbreviate names.arg character   character        50 state.name   "Alabama  …
    ## # … with 1,823 more rows
