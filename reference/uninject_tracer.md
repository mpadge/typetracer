# Remove parameter tracer from one function

This function removes traces previous injected into functions with the
[inject_tracer](https://mpadge.github.io/typetracer/reference/inject_tracer.md)
function.

## Usage

``` r
uninject_tracer(f)
```

## Arguments

- f:

  A function (that is, an object of class "function", and not a
  character string).

## Value

Logical value indicating whether or not tracer was able to be removed
("uninjected").

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
