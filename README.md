
<!-- README.md is generated from README.Rmd. Please edit that file -->
Type Check
----------

Type check allows use of [types](https://github.com/jimhester/types) to automatically add checking code when types are annotated.

### Defining Types

`type_define()` is used to define a new type. The `check` argument specifies a function used to verify the objects type. `type_check` adds the checks to a specific function.

``` r
type.numeric <- type_define(check = is.numeric)

f <- type_check(function(x = ? numeric) x)
f(1)
#> [1] 1
f("txt")
#> Error: `x` is a `character` not a `numeric`.
```

Types are defined as methods of the `type` generic. This means they follow the same properties as normal S3 methods and can be exported and imported to and from packages like all other functions.

The `error` argument is used to specify a custom error message for a type.

``` r
type.numeric <- type_define(
  check = is.numeric,
  error = function(obj_name, obj_value, type) {
     sprintf("%s: '%s' is not a number!", obj_name, obj_value)
  })
f <- type_check(function(x = ? numeric) x)
f("txt")
#> Error: x: 'txt' is not a number!
```

### Packages

When writing a package adding a call to `typeCheck::type_check_package()` anywhere outside a function will add type checks to all functions in the package. Functions without type annotations are unaltered.

This means it is easy to add annotations in a stepwise process to existing packages.

If you are using [roxygen2](https://github.com/klutometis/roxygen) You can use the following `importFrom` statement (or use the equivalent `importFrom()` call directly in the `NAMESPACE` file.)

``` r
f <- function(x = ? numeric) x

#' @importFrom typeCheck type type_define
typeCheck::type_check_package()
```
