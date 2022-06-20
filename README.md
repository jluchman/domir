Tools to Support Relative Importance Analysis
================

[![Stable
version](http://www.r-pkg.org/badges/version-last-release/domir)](https://cran.r-project.org/package=domir)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/domir)](https://cran.r-project.org/package=domir)

# Overview

The `{domir}` package contains functions that apply decomposition-based
relative importance analysis (dominance analysis or Shapley value
decomposition) methods to R functions.

The intention of this package is to allow the decomposition of returned
values and thus relative importance analysis across a wide variety of
data analytic situations an analyst might encounter. The focus of
`{domir}` is on decomposing inputs to predictive models based on fit
statistics that describe them and their fit to data. These
decompositions can be used to determine input relative importance.

# Installation

To install the most recent stable version of `{domir}` from CRAN use:

`install.packages("domir")`

To install the working development version of `{domir}` using the
`{devtools}` package use:

`devtools::install_github("https://github.com/jluchman/domir")`

# What `{domir}` Does

Below, I provide examples of the functionality `{domir}` offers for
evaluating relative importance.

## Comparison with Existing Relative Importance Packages

Fundamentally, `domir::domir` is an extension of the “lmg” type for the
`calc.relimpo` function in the `{relaimpo}` package as well as the
`dominanceAnalysis` function in the [`{dominanceanalysis}`
package](https://github.com/clbustos/dominanceAnalysis) (not on CRAN).

`domir::domir` can replicate the results produced by the above packages
but, as will be seen, requires the user to link the `formula` input to
the prectictive modeling function and fit statistic extractor as well as
filter the result. This difference in structure does make `domir` more
complex to apply but also allows the function a great deal more
flexibility in terms of the kinds of models and fit statistics that can
be dominance analyzed.

Before discussing some of the elements that make `domin` flexible,
consider the following example that shows how `domir` is similar to
existing packages. All three of the dominance analysis results to come
are based on the following linear model:

`lm(mpg ~ am + vs + cyl, data = mtcars)`

The variance explained
![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
will be the focal fit statistic as implemented by `lm`s `summary`
method.

### `{domir}`’s `domir`

``` r
domir(mpg ~ am + vs + cyl, 
      \(fml, data) {
        result <- 
          lm(fml, data = data) |> 
          summary()
        return(result[["r.squared"]])
      }, 
      data = mtcars)
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

In `domir`, the `lm` model is submitted as an anonymous function that
accepts a single argument; a `fomula`. Moreover, the entire pipeline of
the analytic process is supplied from running the `lm`, to calling
`summary` on the `lm` result, to filtering the result to the “r.squared”
element of the summary method call.

`domir` automates taking subsets of the `formula` and submits them, as
the first argument, to the function. In this way, `domir` is a `Map`- or
`lapply`-like function as it receives an object on which to operate
(i.e., the formula) and a function to which to apply to it. `domir` does
expect a numeric scalar to be returned from the function.

Like `lapply`, other arguments (`data = mtcars`) can also be passed to
each call of the function, but must be explicitly built into the
function.

The focus of `domir`’s `print`-ed results focuses on the numerical
results from “General Dominance Values” and “Conditional Dominance
Values” and, a logical matrix of “Complete Dominance Designations”.

### `{relaimpo}`’s `calc.relimp` with “lmg”

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

`{relaimpo}`’s `calc.relimp` is much simpler to implement as it is
specialized for `lm` models and the variance explained
![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
as a fit statistic. There is no need to supply a function to
`calc.relimp` as it requires the use of `lm` as the model it will
implement.

`calc.relimp`’s printed results provide the relative importance
decomposition values (i.e., general dominance values) that match those
obtained from `domir`. In addition, `calc.relimp` reports the average
`lm` coefficients in a way similar to the conditional dominance values
reported by `domir`–an additional and useful result to show the impact
of the models on predicted values.

Note that `{relaimpo}` is not dominance analysis-oriented and does not
report on dominance designations or dominance values other than its
analog of the general dominance values.

### `{dominanceanalysis}`’s `dominanceAnalysis`

``` r
 dominanceanalysis::dominanceAnalysis(
   lm(mpg ~ am + vs + cyl, 
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

`{dominanceanalysis}`’s `dominanceAnalysis` implements dominance
analysis for specific models, of which `lm` is a supported model.
`dominanceAnalysis` accepts a fitted `lm` model as input and uses the
explained variance
![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
as the fit statistic.

`dominanceAnalysis`’s printed output is focused on qualitative dominance
designations but also reports the, magnitude sorted, average
contribution (i.e., general dominance values) values.

`{dominanceanalysis}` has S3 methods for specific models and implements
(pseudo-)![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
values for each. The S3 method approach streamlines the user interface
as all one has to do is submit a fitted model that is supported to get a
result. The number of models supported is relatively small however.

## How `{domir}` Extends on Previous Packages

The intention of `{domir}` is to extend relative importance to new data
analytic situations the user might encounter where a decomposition-based
relative importance method such as dominance analysis could be valuable.

The sections below outline some pertinent examples of specific models
that the `domir` function can accommodate.

### Linear Model Revisited

`domir` is fit statistic agnostic and, as such, one component of its
flexibility is in allowing the user to apply any applicable fit
statistic for a model for the purposes of relative importance analysis.

In this example, the explained variance
![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
is swapped with an alternative, but nonetheless applicable, fit
statistic: the McFadden
pseudo-![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
as implemented by the `{pscl}` package.

Note the use `capture.output`. This are not not strictly necessary but
if not used will print far more output than is needed as `pscl::pR2` is
a rather verbose function and will print a message for each model
fitted. Also note that the `data` argument in this example is submitted
in the function definition instead of as an argument.

``` r
domir(mpg ~ am + vs + cyl, 
      \(fml)
      {(result <- 
          lm(fml, data = mtcars) |> 
          pscl::pR2()
      ) |> capture.output()
        return(result[["McFadden"]])
      })
```

    ## Overall Value:      0.2243283 
    ## 
    ## General Dominance Values:
    ##     General Dominance Standardized Ranks
    ## am         0.04848726    0.2161442     3
    ## vs         0.04970277    0.2215627     2
    ## cyl        0.12613826    0.5622931     1
    ## 
    ## Conditional Dominance Values:
    ##     Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## am      0.06969842     0.05507782    0.020685547
    ## vs      0.09088103     0.05629333    0.001933959
    ## cyl     0.20243215     0.13272881    0.043253806
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?cyl
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA       FALSE
    ## Dmnates?cyl       TRUE       TRUE          NA

Note that this fit statistic produces effectively the same answers, in
terms of qualitative importance inferences about the terms, as that from
the explained variance
![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2").

### Ordered Logistic Regression

`domir` acts like an `lapply` function for models and does not have
built in methods. This is another component of its flexibility as it can
accommodate functions that, to this point, have not been supported in
relative importance analysis. One pertinent example is the `polr`
function from the `{MASS}` package using `peformance::r2`’s default fit
statistic.

``` r
mtcars2 <- data.frame(mtcars, carb2 = as.factor(mtcars$carb))

domir(carb2 ~ am + vs + mpg, 
      \(fml) 
      MASS::polr(fml, data = mtcars2) |>
        performance::r2() |> unlist()
) 
```

    ## Overall Value:      0.5764319 
    ## 
    ## General Dominance Values:
    ##     General Dominance Standardized Ranks
    ## am         0.07067731    0.1226117     3
    ## vs         0.22206005    0.3852321     2
    ## mpg        0.28369455    0.4921562     1
    ## 
    ## Conditional Dominance Values:
    ##     Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## am     0.004737758     0.09192243     0.11537173
    ## vs     0.402858270     0.24330517     0.02001669
    ## mpg    0.383596252     0.30493968     0.16254772
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?mpg
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA          NA
    ## Dmnates?mpg       TRUE         NA          NA

### Decision Trees

`domin` can also accept models that do not produce model coefficients
like `rpart::rpart`.

The fit statistic here is the inverse of the reported error variance
recorded by `rpart`.

``` r
domir(mpg ~ am + vs + cyl, 
      \(fml) {
        result <- 
          rpart::rpart(fml, data = mtcars)
        return(1-result$cptable[nrow(result$cptable), 3])
      }
)
```

    ## Overall Value:      0.7324601 
    ## 
    ## General Dominance Values:
    ##     General Dominance Standardized Ranks
    ## am          0.1199330    0.1637400     3
    ## vs          0.1605074    0.2191346     2
    ## cyl         0.4520197    0.6171254     1
    ## 
    ## Conditional Dominance Values:
    ##     Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## am       0.3597989     0.00000000      0.0000000
    ## vs       0.4409477     0.04057437      0.0000000
    ## cyl      0.7324601     0.33208674      0.2915124
    ## 
    ## Complete Dominance Designations:
    ##             Dmnated?am Dmnated?vs Dmnated?cyl
    ## Dmnates?am          NA         NA       FALSE
    ## Dmnates?vs          NA         NA       FALSE
    ## Dmnates?cyl       TRUE       TRUE          NA

### Multinomial Logistic (softmax) Regression with Extra Features

`domir`, similar to other packages, can combine multiple terms into a
single set as well as use one or more terms as covariate(s) in all model
subsets.

This example outlines another model, `multinom` from the `{nnet}`
package, another function that has not been accommodated in relative
importance packages, that uses sets and all/covariate terms.

In addition, `complete = FALSE` which saves a little computation time
and suppresses reporting complete dominance designations.

``` r
domir(carb2 ~ mpg + gear + am + vs + cyl + disp,
      \(fml) {
        (result <- 
          nnet::multinom(fml, data = mtcars2) |>
          performance::r2()) |> capture.output()
        return(result[["R2"]])
      },
      .set = list(~ am + vs, ~ cyl + disp),
      .all = ~ gear,
      .cpt = FALSE
)
```

    ## Overall Value:      0.9282015 
    ## All Subset Value:  0.1393919 
    ## 
    ## General Dominance Values:
    ##      General Dominance Standardized Ranks
    ## mpg          0.2958544    0.3187394     2
    ## set1         0.1770852    0.1907832     3
    ## set2         0.3158700    0.3403033     1
    ## 
    ## Conditional Dominance Values:
    ##      Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## mpg       0.4452671      0.2553281      0.1869679
    ## set1      0.2886101      0.1365589      0.1060867
    ## set2      0.5769312      0.2753437      0.0953351

### Zero-Inflated Poisson with Wrapper Function

Although `domir` can work directly with modeling functions that accept
standard formula, more complex formulas such as those used by models
such as `zeroinfl` models from the package `{pscl}` can also be
accommodated using wrapper functions.

The below wrapper function`zinfl_wrap` uses the entries in the formula
to create a symmetric count and zero-inflation formulas that will be
submitted to `zeroinfl` model.

In an effort to illustrate what each model submitted to `zeroinfl` looks
like, the model formula for all 7 models is printed before each run.

``` r
library(Formula)

domir(~ fem + mar + kid5, 
      \(fml) {
        result <- 
          as.Formula(fml, fml) |> 
          update(art ~ .) |>
          pscl::zeroinfl(data = pscl::bioChemists) |>
          performance::r2()
        return(result[["R2"]])
      })
```

    ## Overall Value:      0.04922296 
    ## 
    ## General Dominance Values:
    ##      General Dominance Standardized Ranks
    ## fem        0.031066252   0.63113333     1
    ## mar        0.004913445   0.09982017     3
    ## kid5       0.013243265   0.26904650     2
    ## 
    ## Conditional Dominance Values:
    ##      Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## fem     0.027905567    0.031558374    0.033734816
    ## mar     0.003403668    0.005405566    0.005931099
    ## kid5    0.005029077    0.013735387    0.020965332
    ## 
    ## Complete Dominance Designations:
    ##              Dmnated?fem Dmnated?mar Dmnated?kid5
    ## Dmnates?fem           NA        TRUE         TRUE
    ## Dmnates?mar        FALSE          NA        FALSE
    ## Dmnates?kid5       FALSE        TRUE           NA
