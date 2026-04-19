# Summary method for `domir`

Reports dominance designation results from the `domir` class object.

## Usage

``` r
# S3 method for class 'domir'
summary(object, ...)
```

## Arguments

- object:

  an object of class "domir".

- ...:

  further arguments passed to or from other methods. Not used currently.

## Value

The submitted "domir" object with an additional `Strongest_Dominance`
element added.

- `Strongest_Dominance`:

  Matrix comparing the element in the first row to the element in the
  third row. The second row denotes the strongest designation between
  the two names.

## Details

The summary method for class `domir` objects is used for obtaining the
strongest dominance designations (i.e., general, conditional, or
complete) among all pairs of dominance analyzed names.
