# Load traces of parameter types

Load traces of parameter types

## Usage

``` r
load_traces(files = FALSE, quiet = FALSE)
```

## Arguments

- files:

  If `TRUE`, return paths to all temporary files holding trace data.

- quiet:

  If `FALSE`, issue message when no traces found.

## Value

A 'data.frame' of traces, including names of functions and parameters,
and values of each parameter traced in both unevaluated and evaluated
forms.

## Examples

``` r
f <- function (x, y, z, ...) {
    x * x + y * y
}
inject_tracer (f)
val <- f (1:2, 3:4 + 0., a = "blah")
x <- load_traces ()
print (x)
#> # A tibble: 5 × 13
#>   trace_number fn_name fn_call_hash par_name is_named class     typeof    mode  
#>          <int> <chr>   <chr>        <chr>    <lgl>    <I<list>> <chr>     <chr> 
#> 1            0 f       QZeuRHfd     x        FALSE    <chr [1]> integer   numer…
#> 2            0 f       QZeuRHfd     y        FALSE    <chr [1]> double    numer…
#> 3            0 f       QZeuRHfd     z        FALSE    <chr [1]> NULL      NULL  
#> 4            0 f       QZeuRHfd     ...      FALSE    <chr [1]> NULL      NULL  
#> 5            0 f       QZeuRHfd     a        TRUE     <chr [1]> character chara…
#> # ℹ 5 more variables: storage_mode <chr>, length <int>, formal <named list>,
#> #   uneval <I<list>>, eval <I<list>>

# Traces should always be "uninjected":
uninject_tracer (f)
#> [1] TRUE
# Traces may also be removed:
clear_traces ()
```
