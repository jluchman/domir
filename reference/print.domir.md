# Print method for `domir`

Reports formatted results from `domir` class object.

## Usage

``` r
# S3 method for class 'domir'
print(x, .cdl = TRUE, .cpt = TRUE, ...)
```

## Arguments

- x:

  an object of class "domir".

- .cdl:

  Logical. If `TRUE` then conditional dominance statistics will be
  reported.

- .cpt:

  Logical. If `TRUE` then complete dominance proportions will be
  reported.

- ...:

  further arguments passed to
  [`print.default`](https://rdrr.io/r/base/print.default.html).

## Value

The submitted "domir" object, invisibly.

## Details

The print method for class `domir` objects reports out the following
results:

- Value when all elements are included in `obj`.

- Value for the elements included in `.all`, if any.

- Value for the elements included in `.adj`, if any.

- Matrix describing general dominance values, standardized general
  dominance values, and the ranking of the general dominance values.

- Matrix describing the conditional dominance values if `.cdl` is
  `TRUE`.

- Matrix describing the complete dominance designations if `.cpt` is
  `TRUE`.

- If following
  [`summary.domir`](https://jluchman.github.io/domir/reference/summary.domir.md),
  matrix describing the strongest dominance designations between all
  elements if both `.cdl` and `.cpt` are `TRUE`.

The `domir` print method alters dimension names for readability and they
do not display as stored in the `domir` object.
