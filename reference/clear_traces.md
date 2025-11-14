# Clear previous traces

Traces are by default appended to previous traces. This function can be
used to clean those previous ones, to enable subsequent calls to
generate new traces that are not appended to previous ones.

## Usage

``` r
clear_traces()
```

## Value

(Invisibly) A single logical value indicating whether or not traces were
successfully cleared.

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
#> 1            0 f       lKLUWEdX     x        <chr [1]> integ… nume… integer     
#> 2            0 f       lKLUWEdX     y        <chr [1]> double nume… double      
#> 3            0 f       lKLUWEdX     z        <chr [1]> NULL   NULL  NULL        
#> 4            0 f       lKLUWEdX     ...      <chr [1]> NULL   NULL  NULL        
#> 5            0 f       lKLUWEdX     a        <chr [1]> chara… char… character   
#> # ℹ 4 more variables: length <int>, formal <named list>, uneval <I<list>>,
#> #   eval <I<list>>

# Then call 'clear_traces' to remove them:
clear_traces ()
# Trying to load again wil then indicate 'No traces found':
x <- load_traces ()
#> No traces found; first run 'inject_tracer'
# Traces should also always be "uninjected":
uninject_tracer (f)
#> [1] FALSE
```
