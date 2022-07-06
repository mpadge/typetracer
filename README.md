<!-- badges: start -->

[![R-CMD-check](https://github.com/mpadge/typetracer/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/typetracer/actions)
[![codecov](https://codecov.io/gh/mpadge/typetracer/branch/main/graph/badge.svg)](https://codecov.io/gh/mpadge/typetracer)
<!-- badges: end -->

# typetracer

`typetracer` is an R package to trace function parameter types. The main
usage of `typetracer` is to identify parameters used as input to R
functions. Many computer languages have formal type systems, meaning the
types of parameters must be formally declared and encoded. R is
different, and offers no way to specify the expected types of input
parameters. `typetracer` identifies the types of parameters passed to R
functions. The package can trace individual functions or entire
packages, as demonstrated below.

## Installation

The package can be installed with the following command:

    remotes::install_github ("mpadge/typetracer")

Then loaded for use by calling `library`:

    library (typetracer)

## Example \#1 - A Single Function

`typetracer` works by “injecting” tracing code into the body of a
function using [the `inject_tracer()`
function](https://mpadge.github.io/typetracer/reference/inject_tracer.html).
The following function includes four parameters, including `...` to
allow passing of additional and entirely arbitrary parameter types and
values.

    f <- function (x, y, z, ...) {
        x * x + y * y
    }
    inject_tracer (f)

After injecting the `typetracer` code, calls to the function, `f`, will
“trace” each parameter of the function, by capturing both unevaluated
and evaluated representation at the point at which the function is first
called. These values can be accessed with [the `load_traces`
function](https://mpadge.github.io/typetracer/reference/load_traces.html),
which returns a `data.frame` object (in [`tibble`
format](https://tibble.tidyverse.org) with one row for each parameter
from each function call.

    val <- f (x = 1:2, y = 3:4 + 0., a = "blah", b = list (a = 1, b = "b"))
    x <- load_traces ()
    x

    ## # A tibble: 6 × 9
    ##   fn_name fn_call_hash par_name class     storage_mode length formal      uneval
    ##   <chr>   <chr>        <chr>    <I<list>> <chr>         <int> <named lis> <I<li>
    ## 1 f       fi4uBd3C     x        <chr [1]> integer           2 <missing>   <chr> 
    ## 2 f       fi4uBd3C     y        <chr [1]> double            2 <missing>   <chr> 
    ## 3 f       fi4uBd3C     z        <chr [1]> NULL              0 <missing>   <chr> 
    ## 4 f       fi4uBd3C     ...      <chr [1]> NULL              0 <missing>   <chr> 
    ## 5 f       fi4uBd3C     a        <chr [1]> character         1 <NULL>      <chr> 
    ## 6 f       fi4uBd3C     b        <chr [1]> list              2 <NULL>      <chr> 
    ## # … with 1 more variable: eval <I<list>>

That results shows that all parameters of the function, `f()`, were
successfully traced, including the additional parameters, `a` and `b`,
passed as part of the `...` argument. Such additional parameters can be
identified through having a `"formal"` entry of `NULL`, indicating that
they are not part of the formal arguments to the function.

    x$uneval [x$par_name %in% c ("a", "b")]

    ## $a
    ## [1] "blah"
    ## 
    ## $b
    ## [1] "list(a = 1, b = \"b\")"

    x$eval [x$par_name %in% c ("a", "b")]

    ## $a
    ## [1] "blah"
    ## 
    ## $b
    ## $b$a
    ## [1] 1
    ## 
    ## $b$b
    ## [1] "b"

Traces themselves are saved in the temporary directory of the current R
session, and [the `load_traces()`
function](https://mpadge.github.io/typetracer/reference/load_traces.html)
simply loads all traces created in that session. [The function
`clear_traces()`](https://mpadge.github.io/typetracer/reference/clear_traces.html)
removes all traces, so that
[`load_traces()`](https://mpadge.github.io/typetracer/reference/load_traces.html)
will only load new traces produced after that time.

### Uninjecting Traces

It is important after applying [the `inject_tracer()`
function](https://mpadge.github.io/typetracer/reference/inject_tracer.html)
to restore the functions back to their original form through calling
[the obverse `uninject_tracer()`
function](https://mpadge.github.io/typetracer/reference/uninject_tracer.html).
For the function, `r`, above, this simply requires,

    uninject_tracer (f)

    ## [1] TRUE

## Example \#2 - Tracing a Package

This section presents a more complex example tracing all function calls
from [the `rematch` package](https://github.com/MangoTheCat/rematch),
chosen because it has less code than almost any other package on CRAN.
The following single line traces function calls in all examples for the
nominated package. [The `trace_package()`
function](https://mpadge.github.io/typetracer/reference/trace_package.html)
automatically injects tracing code into every function within the
package, so there is no need to explicitly call [the `inject_tracer()`
function](https://mpadge.github.io/typetracer/reference/inject_tracer).

    res <- trace_package ("rematch")
    res

    ## # A tibble: 8 × 9
    ##   fn_name  fn_call_hash par_name class     storage_mode length formal     uneval
    ##   <chr>    <chr>        <chr>    <I<list>> <chr>         <int> <named li> <I<li>
    ## 1 re_match Z2OJe6bC     pattern  <chr [1]> character         1 <missing>  <chr> 
    ## 2 re_match Z2OJe6bC     text     <chr [1]> character         7 <missing>  <chr> 
    ## 3 re_match Z2OJe6bC     perl     <chr [1]> logical           1 <lgl [1]>  <chr> 
    ## 4 re_match Z2OJe6bC     ...      <chr [1]> NULL              0 <missing>  <chr> 
    ## 5 re_match R4HqZIkO     pattern  <chr [1]> character         1 <missing>  <chr> 
    ## 6 re_match R4HqZIkO     text     <chr [1]> character         7 <missing>  <chr> 
    ## 7 re_match R4HqZIkO     perl     <chr [1]> logical           1 <lgl [1]>  <chr> 
    ## 8 re_match R4HqZIkO     ...      <chr [1]> NULL              0 <missing>  <chr> 
    ## # … with 1 more variable: eval <I<list>>

The result contains one line for every parameter passed to every
function call in the examples. [The `trace_package()`
function](https://mpadge.github.io/typetracer/reference/trace_package.html)
also includes an additional parameter, `types`, which defaults to
`c ("examples", "tests")`, so that traces are also by default generated
for all tests included with local source packages.

The final two columns of the result hold the unevaluated and evaluated
representations of each parameter. The first two values of each
demonstrate the difference:

    res$uneval [1:2]

    ## $pattern
    ## [1] "isodate"
    ## 
    ## $text
    ## [1] "dates"

    res$eval [1:2]

    ## $pattern
    ## [1] "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
    ## 
    ## $text
    ## [1] "2016-04-20"       "1977-08-08"       "not a date"       "2016"            
    ## [5] "76-03-02"         "2012-06-30"       "2015-01-21 19:58"

The example first assigns a variable `isodaten` to the first of the
evaluated values, and then calls the function with `pattern = isodaten`.
The second constructs the vector called `dates` with the second of the
evaluated values, then calls the function with `test = dates`.

### Example \#2(a) - Specifying Functions to Trace

[The `trace_package()`
function](https://mpadge.github.io/typetracer/reference/trace_package.html)
also accepts an argument, `functions`, specifying which functions from a
package should be traced. For example,

    x <- trace_package ("stats", functions = c ("sd", "var"))

    ## # A tibble: 10 × 9
    ##    fn_name fn_call_hash par_name class     storage.mode length formal     uneval
    ##    <chr>   <chr>        <chr>    <I<list>> <chr>         <int> <named li> <I<li>
    ##  1 sd      nNbBzFCg     x        <chr [1]> integer           2 <missing>  <chr> 
    ##  2 sd      nNbBzFCg     na.rm    <chr [1]> logical           1 <lgl [1]>  <chr> 
    ##  3 var     kwTgNeUZ     x        <chr [1]> integer           2 <missing>  <chr> 
    ##  4 var     kwTgNeUZ     y        <chr [1]> NULL              0 <NULL>     <chr> 
    ##  5 var     kwTgNeUZ     na.rm    <chr [1]> logical           1 <lgl [1]>  <chr> 
    ##  6 var     kwTgNeUZ     use      <chr [1]> NULL              0 <missing>  <chr> 
    ##  7 var     risTCGmw     x        <chr [1]> integer          10 <missing>  <chr> 
    ##  8 var     risTCGmw     y        <chr [1]> NULL              0 <NULL>     <chr> 
    ##  9 var     risTCGmw     na.rm    <chr [1]> logical           1 <lgl [1]>  <chr> 
    ## 10 var     risTCGmw     use      <chr [1]> NULL              0 <missing>  <chr> 
    ## # … with 1 more variable: eval <I<list>>
