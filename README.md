Tools to Support Relative Importance Analysis
================

[![Stable
version](http://www.r-pkg.org/badges/version-last-release/domir)](https://cran.r-project.org/package=domir)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/domir)](https://cran.r-project.org/package=domir)

# Overview

The `{domir}` package supports the determination of the relative
importance of the inputs (i.e., independent variables, predictors, or
features) in a user’s statistical or machine learning model. The
methodology used by `{domir}` is called *Dominance Analysis* which is
based on a series of pairwise comparisons between the model fit values
ascribed to elements in the model including comparing [*Shapley
values*](https://en.wikipedia.org/wiki/Shapley_value).

The intention of this package is to provide a flexible user interface to
dominance analysis—a relatively assumption-free methodology for
comparing the value of model inputs to prediction. The user interface is
structured such that `{domir}` automates the decomposition of the
returned value and comparisons between model inputs and the user
provides the model inputs, the predictive model into which they are
entered, and returned value from the model to decompose.

# Installation

To install the most recent version of `{domir}` from CRAN use:

`install.packages("domir")`

`{domir}` is also used as the computational engine underlying the
[`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
function for the [{parameters}](https://easystats.github.io/parameters/)
package from the `{easystats}` collection.

# What `{domir}` Does

The primary dominance analysis function `domir` implements the most
computationally intensive and programming heavy parts of dominance
analysis for the user and has relatively few requirements on the
predictive modeling functions with which it can work.

The flexibility of `domir` comes at the cost of more complexity for the
user in terms of setting up a function that accepts the type of input
`domir` will provide (currently only a ‘formula’) and and expects to
receive (currently only a numeric scalar).

Below these ideas are outlined in greater detail in the context of a few
examples. The next section begins the discussion with a more extensive
comparison of `domir` with packages that implement similar methods.

## Comparison with Existing Relative Importance Packages

The `domir` function implements the same method as the “lmg” type for
the `calc.relimpo` function in the `{relaimpo}` package. `domir` can
replicate the results produced by both the above package but, as will be
seen, requires more user input.

To illustrate these points, consider the following example linear
regression on which all three of the dominance analysis results to come
are based:

`lm(mpg ~ am + vs + cyl, data = mtcars)`

Classic dominance analysis uses the variance explained $R^2$ as fit
statistic (i.e., as implemented by `lm`’s `summary` method) and so will
this example.

### `{domir}`’s `domir`

Implementing a ‘classic’ dominance analysis on this linear regression in
`domir` can be inputted as:

``` r
lm_wrapper <-       
  function(formula, data) {
    lm(formula, data = data) |> 
      summary() |>
      _[["r.squared"]]
  }

domir(mpg ~ am + vs + cyl, lm_wrapper, data = mtcars)
```

    ## Overall Value:      0.7619773 
    ## 
    ## General Dominance Values:
    ##     General Dominance Standardized Ranks
    ## am          0.1774892    0.2329324     3
    ## vs          0.2027032    0.2660226     2
    ## cyl         0.3817849    0.5010450     1
    ## 
    ## Conditional Dominance Values:
    ##     Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## am       0.3597989      0.1389842    0.033684441
    ## vs       0.4409477      0.1641982    0.002963748
    ## cyl      0.7261800      0.3432799    0.075894823
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?cyl
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA       FALSE
    ## Dmnates?cyl       TRUE       TRUE          NA

In `domir`, the `lm` model is not submitted directly. Rather, it is
wrapped into a function (i.e., `lm_wrapper`) that, in this case, accepts
two arguments; *formula* or an R formula and *data* a data frame in
which the independent variables in the formula are present. The result
of the `lm` submitted into the `summary` function and the result is then
filtered to just the **r.squared** element and returned.

What `domir` does automate taking subsets of the *formula* and submit
them, repeatedly until all possible subsets have been submitted, to
`lm_wrapper` (see this
[vignette](https://CRAN.R-project.org/package=domir/vignettes/domir_basics.html)
for a conceptual discussion of dominance analysis). In this way, `domir`
is a `Map`- or `lapply`-like function as it receives an object on which
to operate (i.e., the *formula*) and a function to which to apply to it.
`domir` expects a numeric scalar to be returned from the function.

Like `lapply`, other arguments (`data = mtcars`) can also be passed to
each call of the function and must be explicitly built into the wrapper
function.

What is important to note about `domir` that differs from other
dominance analysis-oriented functions discussed below is that `domir`
expects that the user will supply the analysis pipeline linking the
*formula* it passes to the numeric scalar value that it expects. This
‘supply the pipeline’ approach makes `domir` far more flexible than
other implementations but does require the user to think more carefully
about how to structure the pipeline.

Note that the focus of `domir`’s `print`-ed results focuses on the
numerical results from “General Dominance Values” and “Conditional
Dominance Values” and, a logical matrix of “Complete Dominance
Designations”.

See also the (now superseded) `domir::domin` function for another
approach to structuring the input pipeline for dominance analysis.

### `{relaimpo}`’s `calc.relimp` with `type = "lmg"`

`{relaimpo}` is not a dominance analysis software but does produce
general dominance value decomposition for linear regression using the
explained variance $R^2$ in the `calc.relimp` function with the argument
`type = "lmg"`.

``` r
relaimpo::calc.relimp(mpg ~ am + vs + cyl, data = mtcars, type = "lmg")
```

    ## Response variable: mpg 
    ## Total response variance: 36.3241 
    ## Analysis based on 32 observations 
    ## 
    ## 3 Regressors: 
    ## am vs cyl 
    ## Proportion of variance explained by model: 76.2%
    ## Metrics are not normalized (rela=FALSE). 
    ## 
    ## Relative importance metrics: 
    ## 
    ##           lmg
    ## am  0.1774892
    ## vs  0.2027032
    ## cyl 0.3817849
    ## 
    ## Average coefficients for different model sizes: 
    ## 
    ##            1X       2Xs       3Xs
    ## am   7.244939  4.316851  3.026480
    ## vs   7.940476  2.995142  1.294614
    ## cyl -2.875790 -2.795816 -2.137632

`calc.relimp` has a similar to structure to that of `domir` but does not
require a pipeline function. This is because `{relaimpo}` is specialized
and works only with `lm` models and the variance explained $R^2$ as a
fit statistic. `calc.relimp` also allows for multiple methods of
submitting (i.e., correlation matrices, fitted `lm` object, a
`data.frame`) given that it always implements the same model.

`calc.relimp`’s printed results provide relative importance metric
values that match those obtained from `domir` (i.e., the general
dominance values). In addition, `calc.relimp` reports the average `lm`
coefficients across numbers of independent variables/$X$s in a way
similar to the conditional dominance values reported by `domir`—an
additional and useful result to show the impact of inclusion of
different numbers of independent variables on obtained
coefficients/predicted values.

Again, note that `{relaimpo}` is not dominance analysis-oriented and
does not report on dominance designations or dominance values other than
the general dominance values.

# Further Examples

Further examples of `domir`s functionality will be populated on the
[`{domir}` wiki](https://github.com/jluchman/domir/wiki).
