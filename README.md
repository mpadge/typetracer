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

    ## # A tibble: 4 × 8
    ##   fn_name fn_call_hash par_name class    storage_mode length par_uneval par_eval
    ##   <chr>   <chr>        <chr>    <I<list> <chr>         <int> <I<list>>  <I<list>
    ## 1 f       6sKuvFGL     x        <chr>    integer           2 <chr [1]>  <int>   
    ## 2 f       6sKuvFGL     y        <chr>    double            2 <chr [1]>  <dbl>   
    ## 3 f       6sKuvFGL     z        <chr>    NULL              0 <chr [1]>  <NULL>  
    ## 4 f       6sKuvFGL     ...      <chr>    NULL              0 <chr [1]>  <NULL>

Traces themselves are saved in the temporary directory of the current R
session, and the `load_traces()` function simple loads all traces
created in that session. The function `clear_traces()` removes all
traces, so that `load_traces()` will only load new traces produced after
that time.

## Example \#2

This section presents a more complex example tracing parameters for a
selection of functions from [the `rematch`
package](https://github.com/MangoTheCat/rematch), chosen because it has
less code than almost any other package on CRAN. The following single
line traces function calls in all examples for the nominated package:

    res <- trace_package ("rematch")
    res

    ## # A tibble: 8 × 8
    ##   fn_name  fn_call_hash par_name class   storage_mode length par_uneval par_eval
    ##   <chr>    <chr>        <chr>    <I<lis> <chr>         <int> <I<list>>  <I<list>
    ## 1 re_match D3BjJE8V     pattern  <chr>   character         1 <chr [1]>  <chr>   
    ## 2 re_match D3BjJE8V     text     <chr>   character         7 <chr [1]>  <chr>   
    ## 3 re_match D3BjJE8V     perl     <chr>   logical           1 <chr [1]>  <lgl>   
    ## 4 re_match D3BjJE8V     ...      <chr>   NULL              0 <chr [1]>  <NULL>  
    ## 5 re_match ZjsyPBQc     pattern  <chr>   character         1 <chr [1]>  <chr>   
    ## 6 re_match ZjsyPBQc     text     <chr>   character         7 <chr [1]>  <chr>   
    ## 7 re_match ZjsyPBQc     perl     <chr>   logical           1 <chr [1]>  <lgl>   
    ## 8 re_match ZjsyPBQc     ...      <chr>   NULL              0 <chr [1]>  <NULL>

The result contains one line for every parameter passed to every
function call in the examples. The `trace_package()` function also
includes an additional parameter, `types`, which defaults to
`c ("examples", "tests")`, so that traces are also by default generated
for all tests included with local source packages.

The final two columns of the result hold the unevaluated and evaluated
representations of each parameter. The first two values of each
demonstrate the difference:

    res$par_uneval [1:2]

    ## [[1]]
    ## [1] "isodate"
    ## 
    ## [[2]]
    ## [1] "dates"

    res$par_eval [1:2]

    ## [[1]]
    ## [1] "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
    ## 
    ## [[2]]
    ## [1] "2016-04-20"       "1977-08-08"       "not a date"       "2016"            
    ## [5] "76-03-02"         "2012-06-30"       "2015-01-21 19:58"

The example first assigns a variable `isodaten` to the first of the
evaluated values, and then calls the function with `pattern = isodaten`.
The second constructs the vector called `dates` with the second of the
evaluated values, then calls the function with `test = dates`.

## Examples \#3

This example briefly illustrates some examples of tracing parameters
evaluated in non-standards ways. This first examples demonstrates that
parameter values are captured at the initial point of function entry.

    eval_x_late_NSE <- function (x, y) {
        y <- 10 * y
        eval (substitute (x))
    }
    inject_tracer (eval_x_late_NSE)
    eval_x_late_NSE (y + 1, 2:3)

    ## [1] 21 31

    res <- load_traces ()
    res$par_name

    ##         
    ## "x" "y"

    res$par_uneval

    ## [[1]]
    ## [1] "y + 1"
    ## 
    ## [[2]]
    ## [1] "2:3"

    res$par_eval

    ## [[1]]
    ## [1] 3 4
    ## 
    ## [[2]]
    ## [1] 2 3

The parameter `x` is evaluated at the point of function entry as `y + 1`
which, with a value of `y = 2:3`, gives the expected evaluated result of
`x = 3:4`, while the function ultimately returns the expected values of
`(10 * 2:3) + 1`, or `21 31`, because the first line of `y <- 10 * y` is
evaluated prior to substituting the value passed for `x` of `y + 1`.

The second example specifies a default value of `x = y + 1`, with the
actual call passing no value, and thus having `"NULL"` in the
unevaluated version, while evaluated versions remain identical.

    clear_traces () # clear all preceding traces
    eval_x_late_standard <- function (x = y + 1, y, z = y ~ x) {
        y <- 10 * y
        x
    }
    inject_tracer (eval_x_late_standard)
    eval_x_late_standard (, 2:3)

    ## [1] 3 4

    res <- load_traces ()
    res$par_name

    ##             
    ## "x" "y" "z"

    res$par_uneval

    ## [[1]]
    ## [1] "NULL"
    ## 
    ## [[2]]
    ## [1] "2:3"
    ## 
    ## [[3]]
    ## [1] "NULL"

    res$par_eval

    ## [[1]]
    ## [1] 3 4
    ## 
    ## [[2]]
    ## [1] 2 3
    ## 
    ## [[3]]
    ## y ~ x
    ## <environment: 0x55d6891e0d28>
