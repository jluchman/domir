# Dominance analysis methods

Parses input object to obtain a list of names, determines which
combinations of the names are valid, submits valid name combinations to
a function, and computes dominance values based on the returned values
from the function.

## Usage

``` r
domir(.obj, ...)

# S3 method for class 'formula'
domir(
  .obj,
  .fct,
  .set = NULL,
  .wst = NULL,
  .all = NULL,
  .adj = FALSE,
  .cdl = NULL,
  .cpt = NULL,
  .rev = FALSE,
  .cst = NULL,
  .prg = FALSE,
  ...
)

# S3 method for class 'formula_list'
domir(
  .obj,
  .fct,
  .set = NULL,
  .wst = NULL,
  .all = NULL,
  .adj = FALSE,
  .cdl = NULL,
  .cpt = NULL,
  .rev = FALSE,
  .cst = NULL,
  .prg = FALSE,
  ...
)
```

## Arguments

- .obj:

  A `formula` or `formula_list`. Parsed to produce a list of names. All
  valid combinations names from the list are
  [`sapply`](https://rdrr.io/r/base/lapply.html)-ed to `.fct`.

  The combinations of names submitted to `.fct` are formatted to be of
  the same [`class`](https://rdrr.io/r/base/class.html) as `.obj` and
  must be submitted to `.fct` as its first, unnamed argument.

- ...:

  Passes arguments to other methods during method dispatch; passes
  arguments to the function in `.fct` during function execution.

- .fct:

  A [`function`](https://rdrr.io/r/base/function.html) or string
  function name. Applied to all valid combinations of names parsed from
  `.obj`. Must return a length 1/scalar, numeric, atomic vector.

- .set:

  A `list`. Each element of the list must be the same class as `.obj`.
  Elements of the list can be named. Names parsed from the elements of
  the list must also be in `.obj`.

- .wst:

  A `list`. Each element of the list must be the same class as `.obj`.
  Names parsed from the elements of the list must also be in `.obj`.

- .all:

  A `formula` or `formula_list`. Must be the same class as `.obj`.
  Parsed names must also be in `.obj`.

- .adj:

  Logical. If `TRUE` then a model including only an intercept is
  submitted to `.fct` and the value returned is subtracted from the
  values returned from all subsets in the dominance analysis.

- .cdl:

  `NULL`. Depreciated. Use `print(.cdl = FALSE)` to suppress display of
  conditional dominance values.

- .cpt:

  `NULL`. Depreciated. Use `print(.cpt = FALSE)` to suppress display of
  complete dominance values.

- .rev:

  Logical. If `TRUE` then standardized vector, ranks, and complete
  dominance designations are reversed in their interpretation.

- .cst:

  Object of class c("SOCKcluster", "cluster") from `parallel-package`.

  When non-`NULL`, will alter the method for collecting values from all
  combinations of names from using
  [`sapply`](https://rdrr.io/r/base/lapply.html) to
  [`parallel::parSapply`](https://rdrr.io/r/parallel/clusterApply.html).

- .prg:

  Logical. If `TRUE` then a progress bar is displayed during collection
  of values to indicate progress.

## Value

Returns an object of [`class`](https://rdrr.io/r/base/class.html)
"domir" composed of:

- `General_Dominance`:

  Vector of general dominance values.

- `Standardized`:

  Vector of general dominance values normalized to sum to 1.

- `Ranks`:

  Vector of ranks applied to the general dominance values.

- `Conditional_Dominance`:

  Matrix of conditional dominance values. Each row represents a name in
  `.obj`; each column represents a number of names included in `.fct`.

- `Complete_Dominance`:

  Matrix of proportions of subsets where the name in the row has a
  larger value than the name in the column. These proportions determine
  complete dominance when a value of 1 or 0.

- `Value`:

  Value returned by `.fct` with all names included.

- `Value_All`:

  Value of `.fct` associated with names included in `.all`; when `.adj`
  is `TRUE`, this value will be adjusted for `Value_Adjust`.

- `Value_Adjust`:

  Value of `.fct` associated no included names.

- `Call`:

  The matched call.

## Details

### Name Parsing

`.obj` is first parsed into a list of names. How the name list is parsed
depends on `.obj`'s class.

#### `formula`

The `formula` method creates a name list using all terms in the formula.
The terms are obtained using
[`terms.formula`](https://rdrr.io/r/stats/terms.formula.html). All
processing that is normally applied to the right hand side of a formula
is implemented (see [`formula`](https://rdrr.io/r/stats/formula.html)).

A response/left hand side is not required but, if present, is included
in all `formula`s passed to `.fct`.

#### `formula_list`

The
[`formula_list`](https://jluchman.github.io/domir/reference/formula_list.md)
creates a name list out of response-term pairs. The terms are obtained
using `terms.formula` applied to each individual formula in the list.

The `formula_list` methods make it possible to implement dominance
analysis for parameter estimates in multivariate predictive models
(e.g., Luchman, Xie, & Kaplan, 2020).

#### Additional Details

`formula`s and `formula_list` elements are assumed to have an intercept
except if explicitly removed with a `- 1` in the `formula`(s) in `.obj`.
If removed, the intercept will be removed in all `formula`(s) in each
`sapply`-ed subset to `.fct`. If
[`offset`](https://rdrr.io/r/stats/offset.html)s are included, they are
passed, like intercepts, while `sapply`-ing subsets to `.fct`.

### Changing Combination Generation

Parsed names are used independently for creating combinations of names
to submit to`.fct`. When using `.set`, `.wst`, and `.all`, the way
combinations are created changes. Names in `.set`, `.wst`, and `.all`
must also be present in `.obj`.

#### `.set`

`.set` binds together names such that they are considered to be one name
when creating combinations. The names in each `.set` then pool their
returned value together with the other members of their set.

By default, sets are referred to by set names in all output. Unnamed
`.set` list elements are called 'set@' where '@' is an integer
indicating the set's position in the list. The user can give each set in
the `.set` list a name that will be used in place of the default. Names
of `.set`s cannot contain the period character '.'.

`.set` implements the grouped dominance analysis method described by
Luchman (2026).

#### `.wst`

`.wst` binds together names such that they are considered to be one name
when creating combinations with other names that are not in their
`.wst`. All combinations of names of the members of names within a
`.wst` are considered valid. The names in each `.wst` then pool their
returned value together with the other members of their set when
compared to names outside of the `.wst` but do not pool their returned
value when compared to names within their `.wst`.

`.wst` list elements cannot be named.

`.wst` implements the within-group dominance analysis method described
by Luchman (2026). `.wst` and `.set` can be used together.

#### `.all`

`.all` binds together names such that they are considered to be one name
when creating combinations and are always included first, before all
other names. The names in `.all` will then be included in 'all' valid
combinations of names. The names in `.all` are included after the
combination with no names but before any other combinations. The value
associated with `.all` is also removed from the dominance analysis and
is reported along with the overall value including all names.

The `formula` method for `.all` does not allow the formula in `.all` to
have a left hand side.

#### `.adj`

By default, `domir` assumes that the combination of no names has a value
of 0. `.adj` indicates that the there is an 'adjustment' needed such
that the no names combination has a non-0 value and an intercept-only
model should be supplied to `.fct`.

The `formula` method will submit an intercept-only formula to `.fct`.
The `formula_list` method creates a separate, intercept-only subset for
each of the `formula`s in the list. Both the `formula` and
`formula_list` methods will respect the user's removal of an intercept
and or inclusion of an `offset`.

#### Additional Details

All methods submit combinations of names as an object of the same class
as `.obj`. A `formula` in `.obj` will submit all combinations of names
as `formula`s to `.fct`. A `formula_list` in `.obj` will submit all
combinations of subsets of names as `formula_list`s to `.fct`. In the
case that `.fct` requires a different `class` (e.g., a character vector
of names, a
[`Formula::Formula`](https://rdrr.io/pkg/Formula/man/Formula.html) see
[`fmllst2Fml`](https://jluchman.github.io/domir/reference/fmllst2Fml.md))
the subsets of names will have to be processed in `.fct` to obtain the
correct `class`.

The all names will be submitted to `.fct` as the first, unnamed
argument.

### `.fct` as Analysis Pipeline

`.fct` is expected to be a complete analysis pipeline that receives a
subset of names of the same `class` as `.obj` and uses these names in
the `class` as submitted to generate a returned value of the appropriate
type to dominance analyze. Typically, the returned value is a scalar fit
statistic/metric extracted from a predictive model.

At current, only atomic (i.e., non-`list`), numeric scalars (i.e.,
vectors of length 1) are allowed as returned values.

The `.fct` argument is strict about names submitted and returned value
requirements for functions used. A series of checks to ensure the
submitted names and returned value adhere to these requirements. The
checks include whether the `.obj` can be submitted to `.fct` without
producing an error and whether the returned value from `.fct` is a
length 1, atomic, numeric vector. In most circumstances, the user will
have to make their own named or anonymous function to supply as `.fct`
to satisfy the checks.

## Notes

Prior to version 1.1.0, the `formula` method allowed a `formula` to be
submitted to `.adj`. Submitting an intercept-only `formula` as opposed
to a logical has been depreciated and submitting a `formula` with more
than an intercept is defunct.

The `formula` and `formula_list` methods can be used to pass responses,
intercepts, and `offset`s to all combinations of names. If the user
seeks to include other model components integral to estimation (i.e., a
random effect term in
[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)) include them
as [`update`](https://rdrr.io/r/stats/update.formula.html) to the
submitted `formula` or `formula_list` imbedded in `.fct`.

Second-order or higher terms (i.e., interactions like `~ a*b`) are
parsed by default but not used differently from first-order terms for
producing subsets. The values ascribed to such terms may not be valid
unless the user ensures that second-order and higher terms are used
appropriately in `.fct`.

## References

- Luchman, J. N., Lei, X., & Kaplan, S. (2020). Relative Importance
  Analysis with Multivariate Models: Shifting the Focus from Independent
  Variables to Parameter Estimates. Journal of Applied Structural
  Equation Modeling, 4(2), 1-20.
  doi:https://doi.org/10.47263/JASEM.4(2)02

- Luchman, J. N. (2026). Determining Relative Importance with
  Independent Variable Groups: An Alternative Dominance Analysis Method.
  Journal of Behavioral Data Science, 6(1), 1-26.
  doi:https://doi.org/10.35566/jbds/luchman

## Examples

``` r
## Linear model returning r-square
lm_r2 <-
  function(fml, data) {
    lm_res <- lm(fml, data = data)
    summary(lm_res)[["r.squared"]]
 }

domir(mpg ~ am + vs + cyl, lm_r2, data = mtcars)
#> Overall Value:      0.7619773 
#> All Subset Value:   0 
#> Adjustment Value:   0 
#> 
#> General Dominance Values:
#>     General Dominance Standardized Ranks
#> am          0.1774892    0.2329324     3
#> vs          0.2027032    0.2660226     2
#> cyl         0.3817849    0.5010450     1
#> 
#> Conditional Dominance Values:
#>     Include At: 1 Include At: 2 Include At: 3
#> am      0.3597989     0.1389842   0.033684441
#> vs      0.4409477     0.1641982   0.002963748
#> cyl     0.7261800     0.3432799   0.075894823
#> 
#> Complete Dominance Proportions:
#>       > am > vs > cyl
#> am >    NA  0.5     0
#> vs >   0.5   NA     0
#> cyl >  1.0  1.0    NA
#> 

## Linear model including set
domir(
  mpg ~ am + vs + cyl + carb + gear + disp + wt,
  lm_r2,
  .set = list(~ carb + gear, ~ disp + wt),
  data = mtcars
)
#> Overall Value:      0.851596 
#> All Subset Value:   0 
#> Adjustment Value:   0 
#> 
#> General Dominance Values:
#>       General Dominance Standardized Ranks
#> am           0.09446712    0.1109295     5
#> vs           0.10957434    0.1286694     4
#> cyl          0.19767129    0.2321186     3
#> set 1        0.20978183    0.2463396     2
#> set 2        0.24010141    0.2819429     1
#> 
#> Conditional Dominance Values:
#>       Include At: 1 Include At: 2 Include At: 3 Include At: 4 Include At: 5
#> am        0.3597989    0.07688044    0.01944026   0.010342235  0.0058737118
#> vs        0.4409477    0.09276443    0.01167477   0.001799976  0.0006848322
#> cyl       0.7261800    0.19877978    0.04304518   0.015251991  0.0050994979
#> set 1     0.7343966    0.20916653    0.05695739   0.030887988  0.0175006137
#> set 2     0.7809306    0.24778381    0.08623238   0.051865275  0.0336949874
#> 
#> Complete Dominance Proportions:
#>          > am  > vs > cyl > set 1 > set 2
#> am >       NA 0.625  0.25       0       0
#> vs >    0.375    NA  0.00       0       0
#> cyl >   0.750 1.000    NA       0       0
#> set 1 > 1.000 1.000  1.00      NA       0
#> set 2 > 1.000 1.000  1.00       1      NA
#> 

## Multivariate regression with multivariate r-square and
## all subsets variable
if (requireNamespace("performance", quietly = TRUE)) {
  mlm_rxy <-
    function(fml, data) {
      mlm_res <- lm(fml, data = data)
      performance::r2_mlm(mlm_res)[["R_xy"]]
    }

  domir(
    cbind(wt, mpg) ~ vs + cyl + am + carb,
    mlm_rxy,
    .all = ~ carb,
    data = mtcars,
    dvnames = c("wt", "mpg")
  )
}
#> Error in (function (fml, data) {    mlm_res <- lm(fml, data = data)    performance::r2_mlm(mlm_res)[["R_xy"]]})(cbind(wt, mpg) ~ vs + cyl + am + carb, data = structure(list(    mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8,     19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4,     33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8,     19.7, 15, 21.4), cyl = c(6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6,     8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8, 8, 8, 8, 4, 4, 4, 8, 6,     8, 4), disp = c(160, 160, 108, 258, 360, 225, 360, 146.7,     140.8, 167.6, 167.6, 275.8, 275.8, 275.8, 472, 460, 440,     78.7, 75.7, 71.1, 120.1, 318, 304, 350, 400, 79, 120.3, 95.1,     351, 145, 301, 121), hp = c(110, 110, 93, 110, 175, 105,     245, 62, 95, 123, 123, 180, 180, 180, 205, 215, 230, 66,     52, 65, 97, 150, 150, 245, 175, 66, 91, 113, 264, 175, 335,     109), drat = c(3.9, 3.9, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69,     3.92, 3.92, 3.92, 3.07, 3.07, 3.07, 2.93, 3, 3.23, 4.08,     4.93, 4.22, 3.7, 2.76, 3.15, 3.73, 3.08, 4.08, 4.43, 3.77,     4.22, 3.62, 3.54, 4.11), wt = c(2.62, 2.875, 2.32, 3.215,     3.44, 3.46, 3.57, 3.19, 3.15, 3.44, 3.44, 4.07, 3.73, 3.78,     5.25, 5.424, 5.345, 2.2, 1.615, 1.835, 2.465, 3.52, 3.435,     3.84, 3.845, 1.935, 2.14, 1.513, 3.17, 2.77, 3.57, 2.78),     qsec = c(16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84,     20, 22.9, 18.3, 18.9, 17.4, 17.6, 18, 17.98, 17.82, 17.42,     19.47, 18.52, 19.9, 20.01, 16.87, 17.3, 15.41, 17.05, 18.9,     16.7, 16.9, 14.5, 15.5, 14.6, 18.6), vs = c(0, 0, 1, 1, 0,     1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,     0, 1, 0, 1, 0, 0, 0, 1), am = c(1, 1, 1, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1,     1, 1, 1, 1), gear = c(4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3,     3, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 4, 5, 5, 5, 5, 5,     4), carb = c(4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4,     4, 4, 1, 2, 1, 1, 2, 2, 4, 2, 1, 2, 2, 4, 6, 8, 2)), row.names = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive", "Hornet Sportabout", "Valiant", "Duster 360", "Merc 240D", "Merc 230", "Merc 280", "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC", "Cadillac Fleetwood", "Lincoln Continental", "Chrysler Imperial", "Fiat 128", "Honda Civic", "Toyota Corolla", "Toyota Corona", "Dodge Challenger", "AMC Javelin", "Camaro Z28", "Pontiac Firebird", "Fiat X1-9", "Porsche 914-2", "Lotus Europa", "Ford Pantera L", "Ferrari Dino", "Maserati Bora", "Volvo 142E"), class = "data.frame"), dvnames = c("wt", "mpg")): unused argument (dvnames = c("wt", "mpg"))

## Named sets
domir(
  mpg ~ am + gear + cyl + vs + qsec + drat,
  lm_r2,
  data = mtcars,
  .set =
    list(
      trns = ~ am + gear,
      eng = ~ cyl + vs,
      misc = ~ qsec + drat
    )
)
#> Overall Value:      0.7723016 
#> All Subset Value:   0 
#> Adjustment Value:   0 
#> 
#> General Dominance Values:
#>      General Dominance Standardized Ranks
#> trns         0.1568309    0.2030695     3
#> eng          0.3559983    0.4609576     1
#> misc         0.2594724    0.3359729     2
#> 
#> Conditional Dominance Values:
#>      Include At: 1 Include At: 2 Include At: 3
#> trns     0.3598419    0.08105688   0.029593970
#> eng      0.7282929    0.28022429   0.059477805
#> misc     0.5921951    0.18369835   0.002523669
#> 
#> Complete Dominance Proportions:
#>        > trns > eng > misc
#> trns >     NA     0    0.5
#> eng >     1.0    NA    1.0
#> misc >    0.5     0     NA
#> 

## Linear model returning AIC
lm_aic <-
  function(fml, data) {
    lm_res <- lm(fml, data = data)
    AIC(lm_res)
 }

domir(
  mpg ~ am + carb + cyl,
  lm_aic,
  .adj = TRUE,
  .rev = TRUE,
  data = mtcars
 )
#> Overall Value:      161.392 
#> All Subset Value:   0 
#> Adjustment Value:   208.7555 
#> 
#> General Dominance Values:
#>      General Dominance Standardized Ranks
#> am          -11.392847    0.7594593     2
#> carb         -8.862961    0.8128735     3
#> cyl         -27.107674    0.4276672     1
#> 
#> Conditional Dominance Values:
#>      Include At: 1 Include At: 2 Include At: 3
#> am        196.4844     -13.71691     -216.9460
#> carb      199.1807     -11.18702     -214.5825
#> cyl       169.3064     -29.43173     -221.1977
#> 
#> Complete Dominance Proportions:
#>        > am > carb > cyl
#> am >     NA     -1     0
#> carb >    0     NA     0
#> cyl >    -1     -1    NA
#> 

## 'systemfit' with 'formula_list' method returning AIC
if (requireNamespace("systemfit", quietly = TRUE)) {
  domir(
    formula_list(mpg ~ am + cyl + carb, qsec ~ wt + cyl + carb),
    function(fml) {
      res <- systemfit::systemfit(fml, data = mtcars)
      AIC(res)
    },
    .adj = TRUE, .rev = TRUE
  )
}
#> Overall Value:      249.6543 
#> All Subset Value:   0 
#> Adjustment Value:   331.5366 
#> 
#> General Dominance Values:
#>           General Dominance Standardized Ranks
#> mpg~am           -15.325244    0.8128381     2
#> mpg~cyl          -24.528785    0.7004385     1
#> mpg~carb          -7.562994    0.9076358     5
#> qsec~wt           -7.100567    0.9132832     6
#> qsec~cyl         -14.587951    0.8218424     3
#> qsec~carb        -12.776752    0.8439620     4
#> 
#> Conditional Dominance Values:
#>           Include At: 1 Include At: 2 Include At: 3 Include At: 4 Include At: 5
#> mpg~am       -27.267193   -17.2155297    -13.310890    -11.614273    -10.798286
#> mpg~cyl      -34.140455   -32.7126537    -29.580119    -24.066568    -16.893677
#> mpg~carb      -3.557813    -7.6551343     -9.517313     -8.955999     -7.533045
#> qsec~wt        4.729558    -0.3628639     -4.385376     -8.017913    -12.972984
#> qsec~cyl      -5.955071   -10.0887891    -12.887191    -14.797171    -17.965001
#> qsec~carb    -10.041693   -11.3383451    -12.630015    -13.052981    -13.479836
#>           Include At: 6
#> mpg~am       -11.745296
#> mpg~cyl       -9.779235
#> mpg~carb      -8.158657
#> qsec~wt      -21.593823
#> qsec~cyl     -25.834484
#> qsec~carb    -16.117639
#> 
#> Complete Dominance Proportions:
#>             > mpg~am > mpg~cyl > mpg~carb > qsec~wt > qsec~cyl > qsec~carb
#> mpg~am >          NA   -0.1250    -0.7500   -0.6875    -0.5000     -0.5000
#> mpg~cyl >    -0.8750        NA    -0.9375   -0.9375    -0.8125     -0.8125
#> mpg~carb >   -0.2500   -0.0625         NA   -0.6875    -0.3125     -0.3125
#> qsec~wt >    -0.3125   -0.0625    -0.3125        NA    -0.0625     -0.3750
#> qsec~cyl >   -0.5000   -0.1875    -0.6875   -0.9375         NA     -0.5000
#> qsec~carb >  -0.5000   -0.1875    -0.6875   -0.6250    -0.5000          NA
#> 
```
