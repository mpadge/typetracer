<!-- badges: start -->

[![R-CMD-check](https://github.com/mpadge/typetracer/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/typetracer/actions)
[![codecov](https://codecov.io/gh/mpadge/typetracer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mpadge/typetracer)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/typetracer)](https://cran.r-project.org/package=typetracer/)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/typetracer?color=orange)](https://cran.r-project.org/package=typetracer)
<!-- badges: end -->

# typetracer

`typetracer` is an R package to trace function parameter types. The R
language includes [a set of defined
types](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Basic-types),
but the language itself is [“absurdly
dynamic”](https://dl.acm.org/doi/pdf/10.1145/3340670.3342426)[1], and
lacks any way to specify which types are expected by any expression. The
`typetracer` package enables code to be traced to extract detailed
information on the properties of parameters passed to R functions.
`typetracer` can trace individual functions or entire packages, as
demonstrated below.

## Installation

The package can be installed with the following command:

    remotes::install_github ("mpadge/typetracer")

Then loaded for use by calling `library`:

    library (typetracer)

## Example \#1 - A Single Function

`typetracer` works by “injecting” tracing code into the body of a
function using [the `inject_tracer()`
function](https://mpadge.github.io/typetracer/reference/inject_tracer.html).
Locally-defined functions can be traced by simply passing the functions
directly to `inject_tracer()`. The following example includes four
parameters, including `...` to allow passing of additional and entirely
arbitrary parameter types and values.

    f <- function (x, y, z, ...) {
        x * x + y * y
    }
    inject_tracer (f)

After injecting the `typetracer` code, calls to the function, `f`, will
“trace” each parameter of the function, by capturing both unevaluated
and evaluated representations at the point at which the function is
first called. These values can be accessed with [the `load_traces`
function](https://mpadge.github.io/typetracer/reference/load_traces.html),
which returns a `data.frame` object (in [`tibble`
format](https://tibble.tidyverse.org)) with one row for each parameter
from each function call.

    val <- f (
        x = 1:2,
        y = 3:4 + 0.,
        a = "blah",
        b = list (a = 1, b = "b"),
        f = a ~ b
    )
    x <- load_traces ()
    x

    ## # A tibble: 7 × 12
    ##   trace_nu…¹ fn_name fn_ca…² par_n…³ class typeof mode  stora…⁴ length formal   
    ##        <int> <chr>   <chr>   <chr>   <I<l> <chr>  <chr> <chr>    <int> <named l>
    ## 1          0 f       BGOMYd… x       <chr> integ… nume… integer      2 <missing>
    ## 2          0 f       BGOMYd… y       <chr> double nume… double       2 <missing>
    ## 3          0 f       BGOMYd… z       <chr> NULL   NULL  NULL         0 <missing>
    ## 4          0 f       BGOMYd… ...     <chr> NULL   NULL  NULL         0 <missing>
    ## 5          0 f       BGOMYd… a       <chr> chara… char… charac…      1 <NULL>   
    ## 6          0 f       BGOMYd… b       <chr> list   list  list         2 <NULL>   
    ## 7          0 f       BGOMYd… f       <chr> langu… call  langua…      3 <NULL>   
    ## # … with 2 more variables: uneval <I<list>>, eval <I<list>>, and abbreviated
    ## #   variable names ¹​trace_number, ²​fn_call_hash, ³​par_name, ⁴​storage_mode

Each row of the result returned by `load_traces()` represents one
parameter passed to one function call. Each function call itself
represents a single “trace” as enumerated by the `trace_number` column,
and also uniquely identified by an arbitrary function call hash
(`fn_call_hash`). The remaining columns of the trace data define the
properties of each parameter, `p`, as:

1.  `par_name`: Name of parameter.
2.  `class`: List of classes of parameter.
3.  `typeof`: Result of `typeof(p)`.
4.  `mode`: Result of `mode(p)`.
5.  `storage_mode`: Result of `storage.mode(p)`.
6.  `length`: Result of `length(p)`.
7.  `formal`: Result of `formals(f)[["p"]]`, as named list item with
    default value where specified.
8.  `uneval`: Parameters as passed to the function call prior to
    evaluation within function environment.
9.  `eval`: Evaluated version of parameter.

The results above show that all parameters of the function, `f()`, were
successfully traced, including the additional parameters, `a`, `b`, and
`f`, passed as part of the `...` argument. Such additional parameters
can be identified through having a `"formal"` entry of `NULL`,
indicating that they are not part of the formal arguments to the
function.

That result can also be used to demonstrate the difference between the
unevaluated and evaluated forms of parameters:

    x$uneval [x$par_name %in% c ("b", "f")]

    ## $b
    ## [1] "list(a = 1, b = \"b\")"
    ## 
    ## $f
    ## [1] "a ~ b"

    x$eval [x$par_name %in% c ("b", "f")]

    ## $b
    ## $b$a
    ## [1] 1
    ## 
    ## $b$b
    ## [1] "b"
    ## 
    ## 
    ## $f
    ## a ~ b
    ## <environment: 0x55c4851c7198>

Unevaluated parameters are generally converted to equivalent character
expressions.

The `typeof`, `mode`, and `storage_mode` columns are similar, yet may
hold distinct information for certain types of parameters. The
conditions under which these values differ are complex, and depend among
other things on the version of R itself. `typeof` alone should generally
provide sufficient information, although [this list of
differences](https://stackoverflow.com/a/37469255) may provide further
insight into whether the other columns may provide useful additional
information.

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

Because `typetracer` modifies the internal code of functions as defined
within a current R session, we strongly recommend restarting your R
session after using `typetracer`, to ensure expected function behaviour
is restored.

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

    ## # A tibble: 8 × 14
    ##   trace_number trace…¹ fn_name fn_ca…² par_n…³ class typeof mode  stora…⁴ length
    ##          <int> <chr>   <chr>   <chr>   <chr>   <I<l> <chr>  <chr> <chr>    <int>
    ## 1            0 exampl… re_mat… NKOqPl… pattern <chr> chara… char… charac…      1
    ## 2            0 exampl… re_mat… NKOqPl… text    <chr> chara… char… charac…      7
    ## 3            0 exampl… re_mat… NKOqPl… perl    <chr> logic… logi… logical      1
    ## 4            0 exampl… re_mat… NKOqPl… ...     <chr> NULL   NULL  NULL         0
    ## 5            1 exampl… re_mat… kbIZej… pattern <chr> chara… char… charac…      1
    ## 6            1 exampl… re_mat… kbIZej… text    <chr> chara… char… charac…      7
    ## 7            1 exampl… re_mat… kbIZej… perl    <chr> logic… logi… logical      1
    ## 8            1 exampl… re_mat… kbIZej… ...     <chr> NULL   NULL  NULL         0
    ## # … with 4 more variables: formal <named list>, uneval <I<list>>,
    ## #   eval <I<list>>, source <chr>, and abbreviated variable names ¹​trace_source,
    ## #   ²​fn_call_hash, ³​par_name, ⁴​storage_mode

The `data.frame` returned by the `trace_package()` function includes one
more column than the result directly returned by `load_traces()`. This
column is called “source”, and identifies the source-code object which
generated each trace:

    unique (res$source)

    ## [1] "rd_re_match"

Tracing an installed package generally only extracts traces from example
code, as documented in help, or `.Rd`, files. These are identified by
the “rd\_” prefix on the source call, with the `rematch` package
including only one `.Rd` file.

[The `trace_package()`
function](https://mpadge.github.io/typetracer/reference/trace_package.html)
also includes an additional parameter, `types`, which defaults to
`c ("examples", "tests")`, so that traces are also by default generated
for all tests included with local source packages (or for packages
installed to include test files). The “source” column for test files
identifies the names of each test, prefixed with “test\_”. Other than
this column, the results are the same as shown above for
`load_traces()`, with one line for every parameter passed to every
function call in the examples.

### Example \#2(a) - Specifying Functions to Trace

[The `trace_package()`
function](https://mpadge.github.io/typetracer/reference/trace_package.html)
also accepts an argument, `functions`, specifying which functions from a
package should be traced. For example,

    x <- trace_package ("stats", functions = "sd")

    ## # A tibble: 2 × 13
    ##   trace_nu…¹ fn_name fn_ca…² par_n…³ class typeof mode  stora…⁴ length formal   
    ##        <int> <chr>   <chr>   <chr>   <I<l> <chr>  <chr> <chr>    <int> <I<list>>
    ## 1          0 sd      EzasZO… x       <chr> integ… nume… integer      2 <missing>
    ## 2          0 sd      EzasZO… na.rm   <chr> logic… logi… logical      1 <lgl [1]>
    ## # … with 3 more variables: uneval <I<list>>, eval <I<list>>, source <chr>, and
    ## #   abbreviated variable names ¹​trace_number, ²​fn_call_hash, ³​par_name,
    ## #   ⁴​storage_mode

## Prior Art

This package extends on concepts previously developed in other R
packages, notably including:

-   The [`typed` package](https://github.com/moodymudskipper/typed) by
    [@moodymudskipper](https://github.com/moodymudskipper)
-   The [`contractr` package](https://github.com/PRL-PRG/contractr) by
    [@aviralg](https://github.com/aviralg) &
    [@fikovnik](https://github.com/fikovnik)

Plus work explained in detail in this footnote:<br>

[1] Alexi Turcotte & Jan Vitek (2019), *Towards a Type System for R*,
ICOOOLPS ’19: Proceedings of the 14th Workshop on Implementation,
Compilation, Optimization of Object-Oriented Languages, Programs and
Systems. Article No. 4, Pages 1–5,
<https://doi.org/10.1145/3340670.3342426>
