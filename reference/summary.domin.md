# Summary method for `domin`

Reports dominance designation results from the `domin` class object.

## Usage

``` r
# S3 method for class 'domin'
summary(object, ...)
```

## Arguments

- object:

  an object of class "domin".

- ...:

  further arguments passed to or from other methods. Not used currently.

## Value

The originally submitted "domin" object with an additional
`Strongest_Dominance` element added.

- `Strongest_Dominance`:

  Matrix comparing the independent variable in the first row to the
  independent variable in the third row. The second row denotes the
  strongest designation between the two independent variables.

## Details

The summary method for class `domin` is used for obtaining the strongest
dominance designations (i.e., general, conditional, or complete) among
the independent variables.
