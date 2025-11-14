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
#> # A tibble: 5 × 12
#>   trace_number fn_name fn_call_hash par_name class     typeof mode  storage_mode
#>          <int> <chr>   <chr>        <chr>    <I<list>> <chr>  <chr> <chr>       
#> 1            0 f       HfdVZIyv     x        <chr [1]> integ… nume… integer     
#> 2            0 f       HfdVZIyv     y        <chr [1]> double nume… double      
#> 3            0 f       HfdVZIyv     z        <chr [1]> NULL   NULL  NULL        
#> 4            0 f       HfdVZIyv     ...      <chr [1]> NULL   NULL  NULL        
#> 5            0 f       HfdVZIyv     a        <chr [1]> chara… char… character   
#> # ℹ 4 more variables: length <int>, formal <named list>, uneval <I<list>>,
#> #   eval <I<list>>

# Traces should always be "uninjected":
uninject_tracer (f)
#> [1] TRUE
# Traces may also be removed:
clear_traces ()
```
