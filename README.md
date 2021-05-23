Tools to Support Relative Importance Analysis
================

# Overview

The intention of the `{domir}` package is to provide tools that allow
relative importance analysis across a wide variety of practical data
analytic situations. With `{domir}`, if you have a statistical/machine
learning model and an extractor function to obtain a fit metric, you can
conduct a relative importance analysis.

More specifically, `{domir}` is intended to implement a set of flexible
wrapper and helper functions for conducting relative importance analysis
and the current implementation of the package has a focus on dominance
analysis with the `domin` function as a relative importance analysis
method.

For readers looking to familiarize themselves more with the dominance
analysis methodology, a more extensive conceptual discussion of
dominance analysis (which focuses on the Stata version of `domin`) as a
method is available
[here](https://github.com/jluchman/domin/blob/master/README.md).

# Installation

To install the most recent stable version of `domir` from CRAN use:

`install.packages("domir")`

To install the working development version of `{domir}` using the
`devtools` package use:

`devtools::install_github("https://github.com/jluchman/domir")`

# What `{domir}` Does

Before discussing details of the `domir` package, I provide some
examples of what `{domir}` can do.

The focus of this section is on outlining how `domir::domin` extends
existing packages and on the structure of the function.

## Comparison with Existing Relative Importance Packages

Fundamentally, `domir::domin` is an extension of the “lmg” type for the
`calc.relimpo` function in the `{relaimpo}` package as well as the
`dominanceAnalysis` function in the `{dominanceanalysis}` package.

`domir::domin` can provide the same results but is structured
differently and, as will be discussed, is designed for flexibility -
which does come at the expense of complexity.

All three of the models to come are based on the following linear model:

`lm(mpg ~ am + vs + cyl, data = mtcars)`

### `{domir}`’s `domin`

``` r
domin(mpg ~ am + vs + cyl, 
      lm, 
      list(summary, "r.squared"), 
      data = mtcars)
```

    ## Overall Fit Statistic:      0.7619773 
    ## 
    ## General Dominance Statistics:
    ##     General Dominance Standardized Ranks
    ## am          0.1774892    0.2329324     3
    ## vs          0.2027032    0.2660226     2
    ## cyl         0.3817849    0.5010450     1
    ## 
    ## Conditional Dominance Statistics:
    ##        IVs: 1    IVs: 2      IVs: 3
    ## am  0.3597989 0.1389842 0.033684441
    ## vs  0.4409477 0.1641982 0.002963748
    ## cyl 0.7261800 0.3432799 0.075894823
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?cyl
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA       FALSE
    ## Dmnates?cyl       TRUE       TRUE          NA

Note that the `lm` model is submitted in pieces. Specifically, the key
inputs were the formula (`mpg ~ am + vs + cyl`), the indication of the
model to be called (`lm`). I recommend thinking of `domin` as a `Map`-
or `apply`-like function given this structure.

A key difference as you will see from other similar functions is that
`domin` must be supplied with a list outlining extractor function
information (i.e., `list(summary, "r.squared")`; described further in
the [Details](#Details) section).

Like `apply`, other arguments (i.e., `data = mtcars`) can also be passed
to each call of `lm`.

The focus of `domin`’s `print`-ed results focuses on the numerical
results from “General Dominance Statistics” and “Conditional Dominance
Statistics” and, optionally, a logical matrix of “Complete Dominance
Designations”.

### `{relaimpo}`’s `calc.relimp`

``` r
relaimpo::calc.relimp(mpg ~ am + vs + cyl, 
                      data = mtcars, 
                      type = "lmg")
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

`{relaimpo}`’s `calc.relimp` is similar in structure to `domin` but only
allows for `lm`s as well as the standard variance explained
*R*<sup>2</sup> value.

The function also focuses only on the analogous statistics to “General
Dominance Statistics”. In addition, it supplies, by default, average
`lm` coefficients across all models.

### `{dominanceanalysis}`’s `dominanceAnalysis`

``` r
dominanceanalysis::dominanceAnalysis(lm(mpg ~ am + vs + cyl, 
                         data = mtcars))
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

`{dominanceanalysis}`’s `dominanceAnalysis` accepts a fitted `lm` object
(as does `calc.relimp`) and otherwise is similar in structure to both
previous functions. `dominanceAnalysis` also has built-in methods for
specific model implementations like `lm` - which assumes the explained
variance *R*<sup>2</sup> will be used.

`dominanceAnalysis`’s output is focused on qualitative dominance
designations but also reports the “General Dominance Statistic” values.

## How `{domir}` Extends on Previous Packages

The intention of `{domir}` is to extend relative importance to new data
analytic situations the user might encounter where a dominance analysis
could be valuable.

The sections below outline some pertinent examples of specific models
that the `domin` function can accommodate.

### Linear Model Revisited

Because `domin` does not require the use of a specific fit metric, one
important way in which it differs from the previous functions is in that
it allows for different fit metrics than the standard error variance
*R*<sup>2</sup> with models like `lm`. In the example below, the the
`'m` discussed above is dominance analyzed with the McFadden
pseudo-*R*<sup>2</sup> as implemented by the `pscl` package.

Note the use of the `purrr::quietly` function to “mute” `pscl::pR2`.
This is not strictly necessary but if not used will print far more
output than is needed as it is a rather verbose function.

``` r
qui_pR2 <- function(model) purrr::quietly(pscl::pR2)(model)$result

domin(mpg ~ am + vs + cyl, 
      lm, 
      list(qui_pR2, "McFadden"), 
      data = mtcars)
```

    ## Overall Fit Statistic:      0.2243283 
    ## 
    ## General Dominance Statistics:
    ##     General Dominance Standardized Ranks
    ## am         0.04848726    0.2161442     3
    ## vs         0.04970277    0.2215627     2
    ## cyl        0.12613826    0.5622931     1
    ## 
    ## Conditional Dominance Statistics:
    ##         IVs: 1     IVs: 2      IVs: 3
    ## am  0.06969842 0.05507782 0.020685547
    ## vs  0.09088103 0.05629333 0.001933959
    ## cyl 0.20243215 0.13272881 0.043253806
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?cyl
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA       FALSE
    ## Dmnates?cyl       TRUE       TRUE          NA

The ability to use any fit metric that is desired is one part of the
utility of the `domin` function.

Note that this fit metric produces effectively the same answer as that
from the explained variance *R*<sup>2</sup>.

### Ordered Logistic Regression

Additionally, because `domin` does not have built in methods, it can
accommodate functions that, to this point, have not been supported in
relative importance analysis. One pertient example is the `polr`
function from the `MASS` package.

``` r
mtcars2 <- data.frame(mtcars, carb2 = as.factor(mtcars$carb))

domin(carb2 ~ am + vs + mpg, 
      MASS::polr, 
      list(qui_pR2, "McFadden"), 
      data = mtcars2)
```

    ## Overall Fit Statistic:      0.2647682 
    ## 
    ## General Dominance Statistics:
    ##     General Dominance Standardized Ranks
    ## am         0.04221668    0.1594477     3
    ## vs         0.09264306    0.3499026     2
    ## mpg        0.12990844    0.4906497     1
    ## 
    ## Conditional Dominance Statistics:
    ##          IVs: 1     IVs: 2     IVs: 3
    ## am  0.001505741 0.05272927 0.07241503
    ## vs  0.161029601 0.10315565 0.01374394
    ## mpg 0.151278401 0.14042103 0.09802589
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?mpg
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA          NA
    ## Dmnates?mpg       TRUE         NA          NA

The ability to call any model with `domin` is another part of the
function’s utility.

One particularly useful side-effect of relative importance analysis with
non-linear models like `polr`is that the the log-odds transformed
coefficients it produces are difficult to compare to one another whereas
the dominance statistics produced by `domin` are much simpler to
compare.

### Decision Trees

Given its flexibility, `domin` can also accept models that have
traditionally not been dominance analyzed such as those in `{rpart}`.

``` r
domin(mpg ~ am + vs + cyl, 
      rpart::rpart, 
      list(\(model) 
            list(R2 = 1-model$cptable[nrow(model$cptable), 3]), 
            "R2"),
      data = mtcars)
```

    ## Overall Fit Statistic:      0.7324601 
    ## 
    ## General Dominance Statistics:
    ##     General Dominance Standardized Ranks
    ## am          0.1199330    0.1637400     3
    ## vs          0.1605074    0.2191346     2
    ## cyl         0.4520197    0.6171254     1
    ## 
    ## Conditional Dominance Statistics:
    ##        IVs: 1     IVs: 2    IVs: 3
    ## am  0.3597989 0.00000000 0.0000000
    ## vs  0.4409477 0.04057437 0.0000000
    ## cyl 0.7324601 0.33208674 0.2915124
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?cyl
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA       FALSE
    ## Dmnates?cyl       TRUE       TRUE          NA

It is worth noting are that the `fitstat` argument in the above `domin`
call accepted an anonymous function (i.e., in R 4.1’s `\()` notation)
that effectively just extracted the proportion of error from the `rpart`
object. Hence, if the model object returns its own fit statistic, it can
be extracted using an anonymous function.

### Multinomial Logistic (softmax) Regression with Extra Features

`multinom` from the `{nnet}` package is another good example model
function that has not been accommodated in relative importance packages
and is the model function featured in this example.

In this model, several sets of predictors are used as well as a
predictor as covariate in all model subsets.

In addition, `complete = FALSE` which saves a little computation time
and suppresses reporting complete dominance designations.

The `domin` automatically combines the entries in the `formula_overall`,
`sets`, and `all` arguments. The full model formula looks like:

`carb2 ~ mpg + am + vs + cyl + disp + gear`

``` r
domin(carb2 ~ mpg, 
      nnet::multinom, 
      list(qui_pR2, "McFadden"), 
      sets = list(c("am", "vs"), c("cyl", "disp")),
      all = c("gear"),
      complete = FALSE,
      data = mtcars2, 
      trace = FALSE)
```

    ## Overall Fit Statistic:      0.9282015 
    ## All Subsets Fit Statistic:  0.1393919 
    ## 
    ## General Dominance Statistics:
    ##      General Dominance Standardized Ranks
    ## mpg          0.2958544    0.3187394     2
    ## set1         0.1770852    0.1907832     3
    ## set2         0.3158700    0.3403033     1
    ## 
    ## Conditional Dominance Statistics:
    ##         IVs: 1    IVs: 2    IVs: 3
    ## mpg  0.4452671 0.2553281 0.1869679
    ## set1 0.2886101 0.1365589 0.1060867
    ## set2 0.5769312 0.2753437 0.0953351
    ## 
    ## Components of sets:
    ## set1 : am vs 
    ## set2 : cyl disp 
    ## 
    ## All subsets variables: gear

The ability to use sets of variables or covariates in all subsets is not
a novel feature but nonetheless useful to illustrate as combined with
`domin`’s other features.

### Zero-Inflated Poisson with Wrapper Function

Although `domin` can work directly with modeling functions that accept
standard formula, more complex formulas such as those used by models
such as `zeroinfl` models from the package `pscl` can also be
accommodated using wrapper functions.

The below wrapper function`zinfl_wrap` uses the entries in the formula
to create a symmetric count and zero-inflation formulas (with the new
forward pipe operator in R 4.1) that will be submitted to `zeroinfl`
model.

In an effort to illustrate what each model submitted to `zeroinfl` looks
like, the model formula for all 9 models is printed before each run.

``` r
zinfl_wrap <- function(model, ...) {
  zip_terms <- model |> terms() |> attr("term.labels") |> paste(collapse = " + ")
  zip_formula_rhs <- zip_terms |> rep(times = 2) |> paste(collapse = " | ")
  zip_formula_lhs <- (model |> all.vars())[[1]]
  zip_formula <- c(zip_formula_lhs, zip_formula_rhs) |> paste(collapse = " ~ ") |> as.formula()
  print(deparse(zip_formula))
  pscl::zeroinfl(zip_formula, ...)
}

domin(art ~ fem + mar + kid5, 
      zinfl_wrap,
      list(qui_pR2, "McFadden"), 
      data=pscl::bioChemists)
```

    ## [1] "art ~ fem | fem"
    ## [1] "art ~ mar | mar"
    ## [1] "art ~ kid5 | kid5"
    ## [1] "art ~ fem + mar | fem + mar"
    ## [1] "art ~ fem + kid5 | fem + kid5"
    ## [1] "art ~ mar + kid5 | mar + kid5"
    ## [1] "art ~ fem + mar + kid5 | fem + mar + kid5"

    ## Overall Fit Statistic:      0.009101817 
    ## 
    ## General Dominance Statistics:
    ##      General Dominance Standardized Ranks
    ## fem       0.0059812901   0.65715343     1
    ## mar       0.0008482014   0.09319035     3
    ## kid5      0.0022723252   0.24965622     2
    ## 
    ## Conditional Dominance Statistics:
    ##            IVs: 1      IVs: 2      IVs: 3
    ## fem  0.0054489923 0.006059012 0.006435866
    ## mar  0.0005852711 0.000925923 0.001033410
    ## kid5 0.0008854100 0.002350047 0.003581519
    ## 
    ## Complete Dominance Designations:
    ##              Dmnated?fem Dmnated?mar Dmnated?kid5
    ## Dmnates?fem           NA        TRUE         TRUE
    ## Dmnates?mar        FALSE          NA        FALSE
    ## Dmnates?kid5       FALSE        TRUE           NA

Further discussion of how to generate wrapper commands is outlined below
in the [Details](#Details) section.

------------------------------------------------------------------------

# Details

\*\* section in progress \*\*

`{domir}` implements dominance analysis using the `domin` function.

`domin` acts as a `Map`- or `apply`-like function that takes three
‘building block’ arguments:

1.  a formula (`formula_overall`)
2.  a modeling function (`reg`)
3.  list of instructions to call an extractor function that obtains a
    model fit metric (`fitstat`)

The three arguments above effectively invoke the following process
(using the new `|>` forward pipe operator notation in R 4.1).

`reg(formula_overall) |> fitstat()`

Hence, the `reg` function is called using the `formula_overall` as
argument and the results of the `reg` model are are ‘piped’ to
`fitstat`.

## formula

The key argument for `domin` is the formula input and understanding how
it is parsed is important for the effective use of `domin`.

The formula argument is parsed using the `terms` utilities in the
`stats` library. Generally,

and is not ‘data frame aware’. That is, shorthand such as `~ .` will not
work to select variables in a data frame even if a `data` argument is
supplied to the `domin` function.

``` r{formula_example}
formula(mpg ~ .) |> terms(data=mtcars) |> formula()
```

…note all and sets go here as well

many modeling functions that accept a formula that follow the standard
`response ~ terms` format.

`domin` works closely with the `do.call` function and the structure of
the `domin` function is similar as a result. Users are encouraged to
read the documentation for the `do.call` function to understand how
`domin` is implemented and for bug/error checking.

wrapper function that can be used with The format used by domin can be
extended to other functions focused on independent
variable/predictor/feature-based RI can be accommodated with a
additional wrapper functions based on the formula it creates and submits
to modeling functions. Some examples are offered below.

The `{domir}` package is aimed at offering users tools for applying
relative importance/RI analysis to a wide variety of statistical or
machine learning functions. In addition, this package aims to allow
users to apply new fit statistics or metrics to statistical or machine
learning functions that already have RI analysis implementations such as
`lm`.

Currently, the only RI tool implemented in `{domir}` is a dominance
analysis method `domin`.
