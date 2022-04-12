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

    ## # A tibble: 4 × 9
    ##   fn_name fn_call_hash par_name class  storage_mode length par_formal par_uneval
    ##   <chr>   <chr>        <chr>    <I<li> <chr>         <int> <named li> <I<list>> 
    ## 1 f       cQHD9bPx     x        <chr>  integer           2 <missing>  <chr [1]> 
    ## 2 f       cQHD9bPx     y        <chr>  double            2 <missing>  <chr [1]> 
    ## 3 f       cQHD9bPx     z        <chr>  NULL              0 <missing>  <chr [1]> 
    ## 4 f       cQHD9bPx     ...      <chr>  NULL              0 <missing>  <chr [1]> 
    ## # … with 1 more variable: par_eval <I<list>>

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

    ## # A tibble: 8 × 9
    ##   fn_name  fn_call_hash par_name class storage_mode length par_formal par_uneval
    ##   <chr>    <chr>        <chr>    <I<l> <chr>         <int> <named li> <I<list>> 
    ## 1 re_match gi4UbAzj     pattern  <chr> character         1 <missing>  <chr [1]> 
    ## 2 re_match gi4UbAzj     text     <chr> character         7 <missing>  <chr [1]> 
    ## 3 re_match gi4UbAzj     perl     <chr> logical           1 <lgl [1]>  <chr [1]> 
    ## 4 re_match gi4UbAzj     ...      <chr> NULL              0 <missing>  <chr [1]> 
    ## 5 re_match N7dSvbBK     pattern  <chr> character         1 <missing>  <chr [1]> 
    ## 6 re_match N7dSvbBK     text     <chr> character         7 <missing>  <chr [1]> 
    ## 7 re_match N7dSvbBK     perl     <chr> logical           1 <lgl [1]>  <chr [1]> 
    ## 8 re_match N7dSvbBK     ...      <chr> NULL              0 <missing>  <chr [1]> 
    ## # … with 1 more variable: par_eval <I<list>>

The result contains one line for every parameter passed to every
function call in the examples. The `trace_package()` function also
includes an additional parameter, `types`, which defaults to
`c ("examples", "tests")`, so that traces are also by default generated
for all tests included with local source packages.

The final two columns of the result hold the unevaluated and evaluated
representations of each parameter. The first two values of each
demonstrate the difference:

    res$par_uneval [1:2]

    ## $pattern
    ## [1] "isodaten"
    ## 
    ## $text
    ## [1] "dates"

    res$par_eval [1:2]

    ## $pattern
    ## [1] "(?<year>[0-9]{4})-(?<month>[0-1][0-9])-(?<day>[0-3][0-9])"
    ## 
    ## $text
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

    ## [1] "x" "y"

    res$par_uneval

    ## $x
    ## [1] "y + 1"
    ## 
    ## $y
    ## [1] "2:3"

    res$par_eval

    ## $x
    ## [1] 3 4
    ## 
    ## $y
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

    ## [1] "x" "y" "z"

    res$par_uneval

    ## $x
    ## [1] "NULL"
    ## 
    ## $y
    ## [1] "2:3"
    ## 
    ## $z
    ## [1] "NULL"

    res$par_eval

    ## $x
    ## [1] 3 4
    ## 
    ## $y
    ## [1] 2 3
    ## 
    ## $z
    ## y ~ x
    ## <environment: 0x55b07c78d620>

The traces produced by `typetracer` also include a column, `par_formal`,
which contains the default values specified in the definition of
`eval_x_late_standard()`:

    res$par_formal

    ## $x
    ## y + 1
    ## 
    ## $y
    ## 
    ## 
    ## $z
    ## y ~ x

Those three columns of `par_formal`, `par_uneval`, and `par_eval` thus
contain all definitions for all parameters passed to the function
environment, in the three possible states of:

1.  Formal or default values (by definition, in an unevaluated state);
2.  The unevaluated state of any specified parameters; and
3.  The equivalent versions evaluated within the function environmental.
