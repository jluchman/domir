# Print method for `domin`

Reports formatted results from `domin` class object.

## Usage

``` r
# S3 method for class 'domin'
print(x, ...)
```

## Arguments

- x:

  an object of class "domin".

- ...:

  further arguments passed to or from other methods. Not used currently.

## Value

The "domin" object with altered column and row names for conditional and
complete dominance results as displayed in the console.

## Details

The print method for class `domin` objects reports out the following
results:

- Fit statistic for the full model. The fit statistic for the all
  subsets model is reported here if there are any entries in `all`. The
  fit statistic for the constant model is reported here if there are any
  entries in `consmodel`.

- Matrix describing general dominance statistics, standardized general
  dominance statistics, and the ranking of the general dominance
  statistics

- If `conditional` is `TRUE`, matrix describing the conditional
  dominance designations

- If `complete` is `TRUE`, matrix describing the complete dominance
  designations

- If following `summary.domin`, matrix describing the strongest
  dominance designations between all independent variables

- If there are entries in `sets` and/or `all` the terms included in each
  set as well as the terms in all subsets are reported

The `domin` print method alters dimension names for readability and they
do not display as stored in the original `domin` object.
