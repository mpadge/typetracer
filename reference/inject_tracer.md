# Inject parameter tracer into one function

Inject parameter tracer into one function

## Usage

``` r
inject_tracer(f, trace_lists = FALSE)
```

## Arguments

- f:

  A function (that is, an object of class "function", and not a
  character string).

- trace_lists:

  If `TRUE`, trace into any nested list parameters (including
  `data.frame`-type objects), and return type information on each list
  component. The parameter names for these list-components are then
  specified in "dollar-notation", for example 'Orange\$age'.

## Value

Nothing (will error on fail).

## Note

The tracer is defined in the internal `typetracer_header()` function.
This uses an `options` variable defined on package load for the current
`tempdir`, defining a single location where all traced values are
dumped. This is done via `options` to allow both multi-threaded function
calls and calls via callr to be traced.

## Examples

``` r
f <- function (x, y, z, ...) {
    x * x + y * y
}
inject_tracer (f)
val <- f (1:2, 3:4 + 0., a = "blah")
x <- load_traces ()

# Traces should always be "uninjected":
uninject_tracer (f)
#> [1] TRUE
# Traces may also be removed:
clear_traces ()
```
