# Non-Standard Evaluation

This vignette briefly illustrates some examples of tracing parameters
evaluated in non-standard ways. This first examples demonstrates that
parameter values are captured at the initial point of function entry.

``` r

eval_x_late_NSE <- function (x, y) {
    y <- 10 * y
    eval (substitute (x))
}
inject_tracer (eval_x_late_NSE)
eval_x_late_NSE (y + 1, 2:3)
#> [1] 21 31
res <- load_traces ()
res$par_name
#> [1] "x" "y"
res$uneval
#> $x
#> [1] "y + 1"
#> 
#> $y
#> [1] "2:3"
res$eval
#> $x
#> [1] 3 4
#> 
#> $y
#> [1] 2 3
```

The parameter `x` is evaluated at the point of function entry as `y + 1`
which, with a value of `y = 2:3`, gives the expected evaluated result of
`x = 3:4`, while the function ultimately returns the expected values of
`(10 * 2:3) + 1`, or `21 31`, because the first line of `y <- 10 * y` is
evaluated prior to substituting the value passed for `x` of `y + 1`.

The second example specifies a default value of `x = y + 1`, with the
actual call passing no value, and thus having `"NULL"` in the
unevaluated version, while evaluated versions remain identical.

``` r

clear_traces () # clear all preceding traces
eval_x_late_standard <- function (x = y + 1, y, z = y ~ x) {
    y <- 10 * y
    x
}
inject_tracer (eval_x_late_standard)
eval_x_late_standard (, 2:3)
#> [1] 3 4
res <- load_traces ()
res$par_name
#> [1] "x" "y" "z"
res$uneval
#> $x
#> [1] "NULL"
#> 
#> $y
#> [1] "2:3"
#> 
#> $z
#> [1] "NULL"
res$eval
#> $x
#> [1] 3 4
#> 
#> $y
#> [1] 2 3
#> 
#> $z
#> y ~ x
#> <environment: 0x56110b16c398>
```

The traces produced by `typetracer` also include a column, `formal`,
which contains the default values specified in the definition of
`eval_x_late_standard()`:

``` r

res$formal
#> $x
#> y + 1
#> 
#> $y
#> 
#> 
#> $z
#> y ~ x
```

Those three columns of `formal`, `uneval`, and `eval` thus contain all
definitions for all parameters passed to the function environment, in
the three possible states of:

1.  Formal or default values (by definition, in an unevaluated state);
2.  The unevaluated state of any specified parameters; and
3.  The equivalent versions evaluated within the function environmental.
