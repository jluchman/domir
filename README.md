Tools to Support Relative Importance Analysis
================

# domir <img src="man/figures/logo.png" align="right" height="139"/>

[![Stable
version](http://www.r-pkg.org/badges/version-last-release/domir)](https://cran.r-project.org/package=domir)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/domir)](https://cran.r-project.org/package=domir)

# Overview

The **domir** package supports determining the relative importance of
inputs (i.e., independent variables, predictors, or features referred to
as *names* in the package) in a user’s statistical or machine learning
model.

The intention of this package is to provide a flexible user interface to
*Dominance Analysis*—a relatively assumption-free methodology for
comparing the predictive value, usefulness, or importance associated
with of model inputs/names.

Dominance analysis resolves the indeterminancy of ascribing the value
returned by a predictive modeling function to inputs/names when it is
not possible to do so analytically. The most common use case for the
application of dominance analysis is in comparing inputs/names in terms
of their contribution to a predictive model’s fit statistic or metric.

# Installation

To install the most recent version of **domir** from CRAN use:

`install.packages("domir")`

**domir** is also used as the computational engine underlying the
[`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
function for the
[**parameters**](https://easystats.github.io/parameters/) package in the
[**easystats**](https://easystats.github.io/easystats/)
framework/collection.

# What **domir** Does

`domir` computes three different sets of results based on a set of
inputs/names and the values returned from a function like this linear
regression model.

`lm(mpg ~ am + vs + cyl, data = mtcars)`

Using the variance explained $R^2$ as fit statistic as implemented by
`lm`‘s `summary` method as the returned value, `domir` can implement a
’classic’ dominance analysis[^1] as:

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

`domir` requires the set of inputs/names, submitted as a `formula` or a
specialized
[`formula_list`](https://jluchman.github.io/domir/reference/formula_list.html)
object, and a function that accepts the input/names and returns a
single, numeric value.

Note the use of a wrapper function, `lm_wrapper`, that accepts a
`formula` and returns the $R^2$. These ‘analysis pipeline’ wrapper
functions are necessary for the effective use of `domir` and the ability
to use them to adapt predictive models to the computational engine used
by `domir` makes this package able to apply to almost any model.

`domir` by default reports on complete dominance proportions,
conditional dominance values, and general dominance values.

Complete dominance proportions are the proportion of subsets of
inputs/names where the name in the row obtains a bigger value than the
name in the column.

Conditional dominance values are the average value associated with the
name when included sequentially at each possible position in the
sequence of name slots.

General dominance values are the average value associated with the name
across all possible ways of including the name in the sequence of all
names. These values are also equivalent to the [Shapley
Value](https://en.wikipedia.org/wiki/Shapley_value) for each name.

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

**relaimpo** is for importance analysis with linear regression with
variance explained $R^2$ as a fit statistic and is optimized to analyze
that model-fit statistic pairing across multiple ways of submitting data
(i.e., correlation matrices, fitted `lm` object, a `data.frame`).

The `dominanceAnalysis` function in **dominanceAnalysis** produces many
of the same statistics as `domir` as in the example below:

``` r
dominanceanalysis::dominanceAnalysis(lm(mpg ~ am + vs + cyl, data = mtcars))
```

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

**dominanceAnalysis** is for the relative importance of specific
model-fit statistic pairs as it is implemented using S3 methods focused
on model types to implement similar to how
`parameters::dominance_analysis` works but using a custom implementation
not dependent on the **insight** package to parse model components and
implement the methodology.

# Further Examples

Further examples of `domir`s functionality will be populated on the
[**domir** wiki](https://github.com/jluchman/domir/wiki).

[^1]: see this
    [vignette](https://CRAN.R-project.org/package=domir/vignettes/domir_basics.html)
    for a conceptual discussion of dominance analysis
