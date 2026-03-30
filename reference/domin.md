# Dominance analysis supporting `formula`-based modeling functions

Computes dominance statistics for predictive modeling functions that
accept a [`formula`](https://rdrr.io/r/stats/formula.html).

## Usage

``` r
domin(
  formula_overall,
  reg,
  fitstat,
  sets = NULL,
  all = NULL,
  conditional = TRUE,
  complete = TRUE,
  consmodel = NULL,
  reverse = FALSE,
  ...
)
```

## Arguments

- formula_overall:

  An object of class [`formula`](https://rdrr.io/r/stats/formula.html)
  or that can be coerced to class `formula` for use in the modeling
  function in `reg`. The [`terms`](https://rdrr.io/r/stats/terms.html)
  on the right hand side of this formula are used as separate entries to
  the dominance analysis.

  A valid `formula_overall` entry is necessary, even if only submitting
  entries in `sets`, to define a valid left hand side of the prediction
  equation (see examples). The function called in `reg` must accept one
  or more responses on the left hand side.

- reg:

  A function implementing the predictive (or "reg"ression) model called.

  String function names (e.g., "lm"), function names (e.g., `lm`), or
  anonymous functions (e.g., `function(x) lm(x)`) are acceptable
  entries. This argument's contents are passed to
  [`do.call`](https://rdrr.io/r/base/do.call.html) and thus any function
  call `do.call` would accept is valid.

  The predictive model in `reg` must accept a `formula` object as its
  first argument or must be adapted to do so with a wrapper function.

- fitstat:

  List providing arguments to call a fit statistic extracting function
  (see details). The `fitstat` list must be of at least length two.

  The first element of `fitstat` must be a function implementing the fit
  statistic extraction. String function names (e.g., "summary"),
  function names (e.g., `summary`), or anonymous functions (e.g.,
  `function(x) summary(x)`) are acceptable entries. This element's
  contents are passed to
  [`do.call`](https://rdrr.io/r/base/do.call.html) and thus any function
  call `do.call` would accept is valid.

  The second element of `fitstat` must be the named element of the list
  or vector produced by the fit extractor function called in the first
  element of `fitstat`. This element must be a string (e.g.,
  "r.squared").

  All list elements beyond the second are submitted as additional
  arguments to the fit extractor function call.

  The fit statistic extractor function in the first list element of
  `fitstat` must accept the model object produced by the predictive
  modeling function in `reg` as its first argument or be adapted to do
  so with a wrapper function.

  The fit statistic produced must be scalar valued (i.e., vector of
  length 1).

- sets:

  A list with each element comprised of vectors containing
  variable/factor names or `formula` coercible strings.

  Each separate list element-vector in `sets` is concatenated (when the
  list element-vector is of length \> 1) and used as an entry to the
  dominance analysis along with the terms in `formula_overall`.

- all:

  A vector of variable/factor names or `formula` coercible strings. The
  entries in this vector are concatenated (when of length \> 1) but are
  not used in the dominance analysis. Rather the value of the fit
  statistic associated with these terms is removed from the dominance
  analysis; this vector is used like a set of covariates.

  The entries in `all` are removed from and considered an additional
  component that explains the fit metric. As a result, the general
  dominance statistics will no longer sum to the overall fit metric and
  the standardized vector will no longer sum to 1.

- conditional:

  Logical. If `FALSE` then conditional dominance matrix is not computed.

  If conditional dominance is not desired as an importance criterion,
  avoiding computing the conditional dominance matrix can save
  computation time.

- complete:

  Logical. If `FALSE` then complete dominance matrix is not computed.

  If complete dominance is not desired as an importance criterion,
  avoiding computing complete dominance designations can save
  computation time.

- consmodel:

  A vector of variable/factor names, `formula` coercible strings, or
  other formula terms (i.e., 1 to indicate an intercept). The entries in
  this vector are concatenated (when of length \> 1) and, like the
  entries of `all`, are not used in the dominance analysis; this vector
  is used as an adjustment to the baseline value of the overall fit
  statistic.

  The use of `consmodel` changes the interpretation of the the general
  and conditional dominance statistics. When `consmodel` is used, the
  general and conditional dominance statistics are reflect the
  difference between the constant model and the overall fit statistic
  values.

  Typical usage of `consmodel` is to pass "1" to set the intercept as
  the baseline and control for its value when the baseline model's fit
  statistic value is not 0 (e.g., if using the AIC or BIC as a fit
  statistic; see examples).

  As such, this vector is used to set a baseline for the fit statistic
  when it is non-0.

- reverse:

  Logical. If `TRUE` then standardized vector, ranks, and complete
  dominance designations are reversed in their interpretation.

  This argument should be changed to `TRUE` if the fit statistic used
  decreases with better fit to the data (e.g., AIC, BIC).

- ...:

  Additional arguments passed to the function call in the `reg`
  argument.

## Value

Returns an object of [`class`](https://rdrr.io/r/base/class.html)
"domin". An object of class "domin" is a list composed of the following
elements:

- `General_Dominance`:

  Vector of general dominance statistics.

- `Standardized`:

  Vector of general dominance statistics normalized to sum to 1.

- `Ranks`:

  Vector of ranks applied to the general dominance statistics.

- `Conditional_Dominance`:

  Matrix of conditional dominance statistics. Each row represents a
  term; each column represents an order of terms.

- `Complete_Dominance`:

  Logical matrix of complete dominance designations. The term
  represented in each row indicates dominance status; the terms
  represented in each columns indicates dominated-by status.

- `Fit_Statistic_Overall`:

  Value of fit statistic for the full model.

- `Fit_Statistic_All_Subsets`:

  Value of fit statistic associated with terms in `all`.

- `Fit_Statistic_Constant_Model`:

  Value of fit statistic associated with terms in `consmodel`.

- `Call`:

  The matched call.

- `Subset_Details`:

  List containing the full model and descriptions of terms in the full
  model by source.

## Details

`domin` automates the computation of all possible combination of entries
to the dominance analysis (DA), the creation of `formula` objects based
on those entries, the modeling calls/fit statistic capture, and the
computation of all the dominance statistics for the user.

`domin` accepts only a "deconstructed" set of inputs and "reconstructs"
them prior to formulating a coherent predictive modeling call.

One specific instance of this deconstruction is in generating the number
of entries to the DA. The number of entries is taken as all the `terms`
from `formula_overall` and the separate list element vectors from
`sets`. The entries themselves are concatenated into a single formula,
combined with the entries in `all`, and submitted to the predictive
modeling function in `reg`. Each different combination of entries to the
DA forms a different `formula` and thus a different model to estimate.

For example, consider this `domin` call:

`domin(y ~ x1 + x2, lm, list(summary, "r.squared"), sets = list(c("x3", "x4")), all = c("c1", "c2"), data = mydata))`

This call records three entries and results in seven (i.e., \\2^3 - 1\\)
different combinations:

1.  x1

2.  x2

3.  x3, x4

4.  x1, x2

5.  x1, x3, x4

6.  x2, x3, x4

7.  x1, x2, x3, x4

`domin` parses `formula_overall` to obtain all the terms in it and
combines them with `sets`. When parsing `formula_overall`, only the
processing that is available in the `stats` package is applied. Note
that `domin` is not programmed to process terms of order \> 1 (i.e.,
interactions/products) appropriately (i.e., only include in the presence
of lower order component terms). `domin` also does not allow `offset`
terms.

From these combinations, the predictive models are constructed and
called. The predictive model call includes the entries in `all`, applies
the appropriate formula, and reconstructs the function itself. The seven
combinations above imply the following series of predictive model calls:

1.  `lm(y ~ x1 + c1 + c2, data = mydata`)

2.  `lm(y ~ x2 + c1 + c2, data = mydata`)

3.  `lm(y ~ x3 + x4 + c1 + c2, data = mydata`)

4.  `lm(y ~ x1 + x2 + c1 + c2, data = mydata`)

5.  `lm(y ~ x1 + x3 + x4 + c1 + c2, data = mydata`)

6.  `lm(y ~ x2 + x3 + x4 + c1 + c2, data = mydata`)

7.  `lm(y ~ x1 + x2 + x3 + x4 + c1 + c2, data = mydata`)

It is possible to use a `domin` with only sets (i.e., no IVs in
`formula_overall`; see examples below). There must be at least two
entries to the DA for `domin` to run.

All the called predictive models are submitted to the fit extractor
function implied by the entries in `fitstat`. Again applying the example
above, all seven predictive models' objects would be individually passed
as follows:

`summary(lm_obj)["r.squared"]`

where `lm_obj` is the model object returned by `lm`.

The entries to `fitstat` must be as a list and follow a specific
structure: `list(fit_function, element_name, ...)`

- `fit_function`:

  First element and function to be applied to the object produced by the
  `reg` function

- `element_name`:

  Second element and name of the element from the object returned by
  `fit_function` to be used as a fit statistic. The fit statistic must
  be scalar-valued/length 1

- `...`:

  Subsequent elements and are additional arguments passed to
  `fit_function`

In the case that the model object returned by `reg` includes its own fit
statistic without the need for an extractor function, the user can apply
an anonymous function following the required format to extract it.

## Notes

`domin` is an R port of the Stata command with the same name (see
Luchman, 2021).

`domin` has been superseded by
[`domir`](https://jluchman.github.io/domir/reference/domir.md).

## References

Luchman, J. N. (2021). Relative importance analysis in Stata using
dominance analysis: domin and domme. The Stata Journal, 21, 2. doi:
10.1177/1536867X211025837.

## Examples

``` r
## Basic linear model with r-square

domin(mpg ~ am + vs + cyl, 
  lm, 
  list("summary", "r.squared"), 
  data = mtcars)
#> Overall Fit Statistic:      0.7619773 
#> 
#> General Dominance Statistics:
#>     General Dominance Standardized Ranks
#> am          0.1774892    0.2329324     3
#> vs          0.2027032    0.2660226     2
#> cyl         0.3817849    0.5010450     1
#> 
#> Conditional Dominance Statistics:
#>        IVs: 1    IVs: 2      IVs: 3
#> am  0.3597989 0.1389842 0.033684441
#> vs  0.4409477 0.1641982 0.002963748
#> cyl 0.7261800 0.3432799 0.075894823
#> 
#> Complete Dominance Designations:
#>             Dmnated?am Dmnated?vs Dmnated?cyl
#> Dmnates?am          NA         NA       FALSE
#> Dmnates?vs          NA         NA       FALSE
#> Dmnates?cyl       TRUE       TRUE          NA
#> 


## Linear model including sets

domin(mpg ~ am + vs + cyl, 
  lm, 
  list("summary", "r.squared"), 
  data = mtcars, 
  sets = list(c("carb", "gear"), c("disp", "wt")))
#> Overall Fit Statistic:      0.851596 
#> 
#> General Dominance Statistics:
#>      General Dominance Standardized Ranks
#> am          0.09446712    0.1109295     5
#> vs          0.10957434    0.1286694     4
#> cyl         0.19767129    0.2321186     3
#> set1        0.20978183    0.2463396     2
#> set2        0.24010141    0.2819429     1
#> 
#> Conditional Dominance Statistics:
#>         IVs: 1     IVs: 2     IVs: 3      IVs: 4       IVs: 5
#> am   0.3597989 0.07688044 0.01944026 0.010342235 0.0058737118
#> vs   0.4409477 0.09276443 0.01167477 0.001799976 0.0006848322
#> cyl  0.7261800 0.19877978 0.04304518 0.015251991 0.0050994979
#> set1 0.7343966 0.20916653 0.05695739 0.030887988 0.0175006137
#> set2 0.7809306 0.24778381 0.08623238 0.051865275 0.0336949874
#> 
#> Complete Dominance Designations:
#>              Dmnated?am Dmnated?vs Dmnated?cyl Dmnated?set1 Dmnated?set2
#> Dmnates?am           NA         NA          NA        FALSE        FALSE
#> Dmnates?vs           NA         NA       FALSE        FALSE        FALSE
#> Dmnates?cyl          NA       TRUE          NA        FALSE        FALSE
#> Dmnates?set1       TRUE       TRUE        TRUE           NA        FALSE
#> Dmnates?set2       TRUE       TRUE        TRUE         TRUE           NA
#> 
#> Components of sets:
#> set1 : carb gear 
#> set2 : disp wt 
#> 


## Multivariate linear model with custom multivariate r-square function 
## and all subsets variable

Rxy <- function(obj, names, data) {
   return(list("r2" = cancor(predict(obj), 
       as.data.frame(mget(names, as.environment(data))))[["cor"]][1]^2)) 
       }
       
domin(cbind(wt, mpg) ~ vs + cyl + am, 
  lm, 
  list(Rxy, "r2", c("mpg", "wt"), mtcars), 
  data = mtcars, 
  all = c("carb"))
#> Overall Fit Statistic:      0.8384378 
#> All Subsets Fit Statistic:  0.3137993 
#> 
#> General Dominance Statistics:
#>     General Dominance Standardized Ranks
#> vs         0.07169528   0.08551056     3
#> cyl        0.21206213   0.25292531     2
#> am         0.24088099   0.28729741     1
#> 
#> Conditional Dominance Statistics:
#>        IVs: 1     IVs: 2      IVs: 3
#> vs  0.1761112 0.03768107 0.001293536
#> cyl 0.4307370 0.17804791 0.027401478
#> am  0.4311156 0.20686678 0.084660605
#> 
#> Complete Dominance Designations:
#>             Dmnated?vs Dmnated?cyl Dmnated?am
#> Dmnates?vs          NA       FALSE      FALSE
#> Dmnates?cyl       TRUE          NA      FALSE
#> Dmnates?am        TRUE        TRUE         NA
#> 
#> All subsets variables: carb


## Sets only

domin(mpg ~ 1, 
  lm, 
  list("summary", "r.squared"), 
  data = mtcars, 
  sets = list(c("am", "vs"), c("cyl", "disp"), c("qsec", "carb")))
#> Overall Fit Statistic:      0.8344278 
#> 
#> General Dominance Statistics:
#>      General Dominance Standardized Ranks
#> set1         0.3197289    0.3831715     2
#> set2         0.3677706    0.4407459     1
#> set3         0.1469282    0.1760826     3
#> 
#> Conditional Dominance Statistics:
#>         IVs: 1     IVs: 2     IVs: 3
#> set1 0.6860825 0.24773691 0.02536743
#> set2 0.7595658 0.29577856 0.04796742
#> set3 0.3092531 0.07493621 0.05659539
#> 
#> Complete Dominance Designations:
#>              Dmnated?set1 Dmnated?set2 Dmnated?set3
#> Dmnates?set1           NA        FALSE           NA
#> Dmnates?set2         TRUE           NA           NA
#> Dmnates?set3           NA           NA           NA
#> 
#> Components of sets:
#> set1 : am vs 
#> set2 : cyl disp 
#> set3 : qsec carb 
#> 
  
## Constant model using AIC

domin(mpg ~ am + carb + cyl, 
  lm, 
  list(function(x) list(aic = extractAIC(x)[[2]]), "aic"), 
  data = mtcars, 
  reverse = TRUE, consmodel = "1")
#> Overall Fit Statistic:      68.57997 
#> Constant Model Fit Statistic:  115.9434 
#> 
#> General Dominance Statistics:
#>      General Dominance Standardized Ranks
#> am          -11.392847    0.2405407     2
#> carb         -8.862961    0.1871265     3
#> cyl         -27.107674    0.5723328     1
#> 
#> Conditional Dominance Statistics:
#>          IVs: 1    IVs: 2     IVs: 3
#> am   -12.271136 -13.71691  -8.190499
#> carb  -9.574847 -11.18702  -5.827017
#> cyl  -39.449099 -29.43173 -12.442191
#> 
#> Complete Dominance Designations:
#>              Dmnated?am Dmnated?carb Dmnated?cyl
#> Dmnates?am           NA         TRUE       FALSE
#> Dmnates?carb      FALSE           NA       FALSE
#> Dmnates?cyl        TRUE         TRUE          NA
#> 
```
