# Translate `formula_list` into `Formula::Formula`

Translates
[`formula_list`](https://jluchman.github.io/domir/reference/formula_list.md)
objects into a
[`Formula::Formula`](https://rdrr.io/pkg/Formula/man/Formula.html)

## Usage

``` r
fmllst2Fml(fmllst, drop_lhs = NULL)
```

## Arguments

- fmllst:

  A `formula_list` classed object.

- drop_lhs:

  An integer vector.

  Used as a selection vector to remove left hand side names prior to
  generating the `Formula` object. This vector must be composed of
  integers (e.g., 1L and not 1).

  This is useful for some `Formulas` that do not have a separate LHS for
  each LHS model part (e.g.,
  [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html)) but
  are required to have separte LHS parts by `formula_list`.

## Value

A [`Formula::Formula`](https://rdrr.io/pkg/Formula/man/Formula.html)
object.
