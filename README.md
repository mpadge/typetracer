# injectr

R package for injecting code into existing functions.

## Usage

```r
> f <- function(x) {
  x*x
}

> inject_code(f, message("f called with x: ", x))

> f(42)
f called with x: 42
[1] 1764
```

```r
> f <- function(x) {
  x*x
}

> inject_code(f, message("f called with x: ", x, " returning: ", returnValue()), "onexit")

> f(42)
f called with x: 42 returning: 1764
[1] 1764
```

## Building

```sh
make build
```

