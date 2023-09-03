# domir 1.1.0

## visible
-   `formula_list` method implemented
    - merger of planned `list` and `Formula::Formula` methods
    - `fmllst2Fml` can be used to translate `formula_list`s to `Formula::Formula`s
    - `offset()`s allowed in `formula_list`; to update in `formula` method
-   `formula` method `.adj` argument is logical
    - `formula`s of the form `~ 1` still accepted but depreciated
    - other `formula` inputs defunct and user pushed to add to `.fct`

## internal

-   Began linting process for code readability

# domir 1.0.1

## bug fixes

-   `formula` method in `domir()` and `domin()` now disallow `offset()` terms
    - Were silently removed and not replaced during parsing.
-   Argument checks for `formula` method produced inconsistent results

# domir 1.0.0

## breaking/major changes

-   New `domir` function
    -   Generic function with three S3 methods: `formula`, `Formula`, and `list`
        -   `Formula` and `list` methods under development
    -   `.obj` submits object on which S3 method dispatch is based
    -   `.fct` submits function that accepts result of same type as `.obj` and returns a scalar
    -   `.set`, `.all` and `.adj` affect `.obj` entries
        - `.wst` argument under development ('within-set' or Owen decomposition)
-   `domin` superseded
    -   No longer under active development; bug fixes only

## internal

-   `dominance_scalar` internal function responsible for computation. `domir` is front-end to computational engine.
    -   Other computational engines planned.  Internal function may be renamed.
    
## bug fixes

-   `summary.domin` not recognizing `reverse = TRUE`.

# domir 0.3.2

## bug fixes

-   removing reference to `dominanceanalysis` package not on CRAN currently given request from CRAN maintainers and CRAN note.

# domir 0.3.1

## visible

-   fixing typos and other editorial errors in *domir_basics* vignette.
-   Extending conditional dominance discussion with graphic.

## bug fixes

-   fixing error in email address in *domir-package* leading to CRAN check warnings.

# domir 0.3.0

## visible

-   `conditional` logical argument to make computing conditional dominance statistics optional
-   initial working version of `summary.domin`
    -   method for computing the strongest dominance designations between independent variables
    -   update to `print.domin` to display strongest dominance designations
-   reorganization and update to *Conceptual Introduction to Dominance Analysis* vignette

## internal

-   looping used to construct many dominance statistics, replaced with matrix-based methods; intended to simplify and expedite computations

## bug fixes

-   update to email in `domin-package` as requested by CRAN maintainers.

# domir 0.2.0

## visible

-   `consmodel` argument to adjust for baseline fit statistic values
-   `reverse` argument to change rank and complete dominance interpretation for fit metrics that decrease with better fit
-   *Conceptual Introduction to Dominance Analysis* vignette added

## internal

-   implemented unit testing framework - covers 82% of `domin`
-   internal use of `reformulate` to construct formulas as opposed to `paste`-ing `+`-es

## bug fixes

-   `Complete_Dominance` computation producing inconsistent complete dominance designations - within-order complete dominance checks not capturing all possible model comparisons

# domir 0.1.0

## visible

-   update to format of `Complete_Dominance` from integers to logicals
-   output and warnings no longer suppressed by default - user must silence noisy functions
-   minimum of two terms/sets to run `domin` (replicates behavior of Stata version)
-   additional checks (response in formula/orders in formula)
-   names of entries in `domin` object changed to syntactic R names when not. Affects entries in:
    -   `Complete_Dominance` matrix
    -   `Conditional_Dominance` matrix \## internal
-   extensive re-write of computation methods
-   pre-allocates container objects to improve performance
-   increased functional-ization of subroutines and overall code readability
-   many internal functions re-named

## bug fixes

-   returned value `.$Subset_Details$Full_Model` now includes variables in `all`
-   `Complete_Dominance` matrix computation
    -   Too many models considered for complete dominance - inconsistent with standard dominance analysis methodology
    -   Fixed error in suppressing complete dominance (i.e., `complete = FALSE`) resulted in error

# domir 0.0.1

## bug fixes

<<<<<<< HEAD
-   `Ensemble_Coordinator` that submitted factor #'s instead of text in R versions \< 4
=======
-   `Ensemble_Coordinator` that submitted factor numbers instead of text in R versions \< 4.0
>>>>>>> 1.0

# domir 0.0.0

-   initial working version of `domin`
-   initial working version of `print.domin`
