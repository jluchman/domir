# Dominance Analysis Methods

# domir

[![Stable
version](http://www.r-pkg.org/badges/version-last-release/domir)](https://cran.r-project.org/package=domir)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/domir)](https://cran.r-project.org/package=domir)

# Overview

**domir** implements several methods to compute dominance analysis[^1].
Dominance analysis is a relative importance analysis approach that
derives conceptually from Shapley values in that it ascribes ‘values’
from some function to inputs (known as ‘names’ in the package) to that
function.

When applied to predictive models, the method compares components of a
fit metric ascribed to each ‘name’ (i.e., independent variable,
predictor, feature, or parameter estimate) to each other ‘name’ in a
pairwise fashion to determine a hierarchy of dominance or relative
importance.

# Installation

To install the most recent version of **domir** from CRAN use:

`install.packages("domir")`

**domir** is also used as the computational engine underlying the
[`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
function for the
[**parameters**](https://easystats.github.io/parameters/) package in
[**easystats**](https://easystats.github.io/easystats/).

# What **`domir`** Does

`domir` computes dominance analysis results based on a set of
inputs/names and the values returned from a function like this linear
regression model.

`lm(mpg ~ am + vs + cyl, data = mtcars)`

Using the variance explained $`R^2`$ as fit statistic as implemented by
`lm`’s `summary` method as the returned value, `domir` produces:

``` r

lm_wrapper <-       
  function(formula, data) {
    lm(formula, data = data) |> 
      summary() |>
      _[["r.squared"]]
  }

domir(mpg ~ am + vs + cyl, lm_wrapper, data = mtcars)
```

``` R
## 
## Overall Value:      0.7619773 
## 
## General Dominance Values:
##     General Dominance Standardized Ranks
## am          0.1774892    0.2329324     3
## vs          0.2027032    0.2660226     2
## cyl         0.3817849    0.5010450     1
## 
## Conditional Dominance Values:
##     Include At: 1 Include At: 2 Include At: 3
## am      0.3597989     0.1389842   0.033684441
## vs      0.4409477     0.1641982   0.002963748
## cyl     0.7261800     0.3432799   0.075894823
## 
## Complete Dominance Proportions:
##       > am > vs > cyl
## am >    NA  0.5     0
## vs >   0.5   NA     0
## cyl >  1.0  1.0    NA
```

`domir` requires a set of inputs/names, submitted as a `formula` or a
specialized
[`formula_list`](https://jluchman.github.io/domir/reference/formula_list.html)
object, and a function that accepts the input/names and returns a
single, numeric value.

The function supplied to `domir` must then be a full ‘analysis pipeline’
function and is necessary for the effective use of `domir`. In fact,
`domir`’s value is in that it allows the use of such pipelines as the
user can define them to apply to almost any predictive model. This
example uses wrapper function, `lm_wrapper`, that accepts a `formula`
and returns the $`R^2`$. A user could use an anonymous function defined
within the `domir` call that has a similar format as an alternative.

# Comparison with Existing Relative Importance Packages

Several other relative importance packages can produce results identical
to `domir` under specific circumstances. I will focus on discussing two
of the most relevant comparison packages below.

The `calc.relimpo` function in the **relaimpo** package with
`type = "lmg"` produces the general dominance values for `lm` as in the
example below:

``` r

relaimpo::calc.relimp(mpg ~ am + vs + cyl, data = mtcars, type = "lmg")
```

``` R
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
```

**relaimpo** is for importance analysis with linear regression with
variance explained $`R^2`$ as a fit statistic and is optimized to
analyze that model-fit statistic pairing across multiple ways of
submitting data (i.e., correlation matrices, fitted `lm` object, a
`data.frame`).

The `dominanceAnalysis` function in **dominanceAnalysis** produces many
of the same statistics as `domir` as in the example below:

``` r

dominanceanalysis::dominanceAnalysis(lm(mpg ~ am + vs + cyl, data = mtcars))
```

``` R
## 
## Dominance analysis
## Predictors: am, vs, cyl 
## Fit-indices: r2 
## 
## * Fit index:  r2 
##     complete conditional general
## am                              
## vs                            am
## cyl    am,vs       am,vs   am,vs
## 
## Average contribution:
##   cyl    vs    am 
## 0.382 0.203 0.177
```

**dominanceAnalysis** is for the relative importance of specific
model-fit statistic pairs as it is implemented using S3 methods focused
on model types to implement similar to how
[`parameters::dominance_analysis`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
works but using a custom implementation not dependent on the **insight**
package to parse model components and implement the methodology.

# Further Examples

Further examples of `domir`s functionality will be populated on the
[**domir** wiki](https://github.com/jluchman/domir/wiki).

[^1]: see this
    [vignette](https://CRAN.R-project.org/package=domir/vignettes/domir_basics.html)
    for a conceptual discussion of dominance analysis
