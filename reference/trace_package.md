# Trace all parameters for all functions in a specified package

Trace all parameters for all functions in a specified package

## Usage

``` r
trace_package(
  package = NULL,
  pkg_dir = NULL,
  functions = NULL,
  types = c("examples", "tests"),
  trace_lists = FALSE
)
```

## Arguments

- package:

  Name of package to be traced (as character value).

- pkg_dir:

  For "types" including "tests", a local directory to the source code of
  the package. (This is needed because installed versions do not
  generally include tests.)

- functions:

  Optional character vector of names of functions to trace. Defaults to
  tracing all functions.

- types:

  The types of code to be run to generate traces: one or both values of
  "examples" or "tests" (as for
  [`tools::testInstalledPackage`](https://rdrr.io/r/tools/testInstalledPackage.html)).
  Note that only tests run via the testthat package can be traced.

- trace_lists:

  If `TRUE`, trace into any nested list parameters (including
  `data.frame`-type objects), and return type information on each list
  component. The parameter names for these list-components are then
  specified in "dollar-notation", for example 'Orange\$age'.

## Value

A `data.frame` of data on every parameter of every function as specified
in code provided in package examples.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- trace_package ("rematch")
res <- trace_package (pkg_dir = "/<path>/<to>/<local>/<pacakge>")
} # }
```
