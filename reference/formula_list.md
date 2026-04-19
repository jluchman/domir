# A [`list`](https://rdrr.io/r/base/list.html) composed of `formulas`

Defines a list object composed of `formula`s. The purpose of this class
of object is to impose structure of the list to ensure that it can be
used to obtain response-term pairs and will be able to be parsed in
[`domir`](https://jluchman.github.io/domir/reference/domir.md).

## Usage

``` r
formula_list(...)
```

## Arguments

- ...:

  `formula`s, possibly named

## Value

A `list` of class `formula_list`.

## Details

The `formula_list` requires that each element of the list is a `formula`
and that each `formula` is unique with a different, non-`NULL` dependent
variable/response.
