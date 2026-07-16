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
#> # A tibble: 5 × 13
#>   trace_number fn_name fn_call_hash par_name is_named class     typeof    mode  
#>          <int> <chr>   <chr>        <chr>    <lgl>    <I<list>> <chr>     <chr> 
#> 1            0 f       SwlKLUEd     x        FALSE    <chr [1]> integer   numer…
#> 2            0 f       SwlKLUEd     y        FALSE    <chr [1]> double    numer…
#> 3            0 f       SwlKLUEd     z        FALSE    <chr [1]> NULL      NULL  
#> 4            0 f       SwlKLUEd     ...      FALSE    <chr [1]> NULL      NULL  
#> 5            0 f       SwlKLUEd     a        TRUE     <chr [1]> character chara…
#> # ℹ 5 more variables: storage_mode <chr>, length <int>, formal <named list>,
#> #   uneval <I<list>>, eval <I<list>>

# Then call 'clear_traces' to remove them:
clear_traces ()
# Trying to load again wil then indicate 'No traces found':
x <- load_traces ()
#> No traces found; first run 'inject_tracer'
# Traces should also always be "uninjected":
uninject_tracer (f)
#> [1] FALSE
```
