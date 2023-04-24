Tools to Support Relative Importance Analysis
================

[![Stable
version](http://www.r-pkg.org/badges/version-last-release/domir)](https://cran.r-project.org/package=domir)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/domir)](https://cran.r-project.org/package=domir)

# Overview

The `{domir}` package contains functions that apply decomposition-based
relative importance analysis methods (*dominance analysis* or *Shapley
value decomposition*) to predictive modeling functions in R.

The intention of this package is to provide a flexible user interface to
dominance analysis—a popular relative importance analysis method that
extends on the rigorous solution concept of Shapley value decomposition
in cooperative game theory. The user interface is structured such that
`{domir}` automates the decomposition of the returned value and
comparisons between model inputs and the user provides the model inputs,
the predictive model into which they are entered, and returned value
from the model to decompose.

# Installation

To install the most recent stable version of `{domir}` from CRAN use:

`install.packages("domir")`

See also the `{domir}`-based `dominance_analysis()` function for the
[{parameters}](https://github.com/easystats/parameters) package from the
`{easystats}`/easyverse.

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

The `domir` function is similar to the “lmg” type for the `calc.relimpo`
function in the `{relaimpo}` package as well as the `dominanceAnalysis`
function in the [`{dominanceanalysis}`
package](https://github.com/clbustos/dominanceAnalysis) (not on CRAN).
`domir` can replicate the results produced by both the above packages
but, as will be seen, requires more user input.

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
    result <- 
      lm(formula, data = data) |> 
      summary()
    return(result[["r.squared"]])
  }

domir(mpg ~ am + vs + cyl, 
      lm_wrapper,
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

In `domir`, the `lm` model is not submitted directly. Rather, it is
wrapped into a function (i.e., `lm_wrapper`) that, in this case, accepts
two arguments; *formula* or an R formula and *data* a data frame in
which the independent variables in the formula are present. The result
of the `lm` is piped (i.e., `|>`) into the `summary` function and the
result is captured as the object *result*. The *result* object is then
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

### `{dominanceanalysis}`’s `dominanceAnalysis`

`{dominanceanalysis}` implements dominance analysis for several
different predictive models including `lm` , `betareg`, and `glm` each
with its own built-in (pseudo-)$R^2$.

``` r
lm_model <- lm(mpg ~ am + vs + cyl, 
      data = mtcars)

dominanceanalysis::dominanceAnalysis(lm_model)
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

`dominanceAnalysis` has a simpler approach than `domir` to get a
‘classic’ dominance analysis as it accepts a fitted `lm` model as input
and requires use of the explained variance $R^2$ as the returned value.
The object returned by `dominanceAnalysis` is large and contains the fit
statistic values from all subsets as well as computed dominance
statistics based on them. Several helper functions are available to
extract specific dominance and other results for printing to the
console.

`dominanceAnalysis`’s default printed output is focused on qualitative
dominance designations but also reports a sorted, average contribution
metric (i.e., general dominance values).

As mentioned above, `{dominanceanalysis}` can be used with around seven
different predictive models and implements a (pseudo-)$R^2$ as returned
values for each. Itis worth noting that the upcoming
`dominance_analysis` function in the `{parameters}` package takes a
similar approach as `{dominanceanalysis}` but works from the `{insight}`
engine linked with `performance::r2` which allows extension to many
different models.

## How `{domir}` Extends on Previous Packages

The intention of `{domir}` is to extend relative importance analysis to
new data analytic situations the user might encounter where a
decomposition-based relative importance method such as dominance
analysis could be valuable.

The sections below outline some pertinent examples that the `domir`
function can accommodate that cannot be r

### Linear Model Revisited

Given that the user supplies the analysis pipeline, one component of
`domir`’s flexibility is in allowing the user to apply any applicable
fit statistic as a returned value for the purposes of relative
importance analysis.

In the example below, the explained variance $R^2$ is swapped with an
alternative, but nonetheless applicable, fit statistic: the McFadden
pseudo-$R^2$ as implemented by the `{pscl}` package.

The example below is more complex than the previous `domir` call as the
analysis pipeline is submitted as an anonymous function with a single
argument (*fml*). In part, this approach is taken to show that the user
*can* submit the function to `domir` in this way. In addition, note that
the `data` argument submitted to the `lm` function is built-into the
analysis pipeline instead of passed as an argument; both are valid
methods of setting arguments to predictive analyses.

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

The use of the McFadden pseudo-$R^2$ has produced effectively the same
answers, in terms of qualitative importance inferences about the
independent variables, as that of the dominance analysis using the
explained variance $R^2$.

It is also worth noting that the use `capture.output` in the anonymous
function was not not strictly necessary. If not used, `domir` will print
far more output than is needed as `pscl::pR2` is a rather verbose
function and will print a message for each model fitted.

### Ordered Logistic Regression

The user-defined analysis pipeline also allows for extending predictive
modeling to effectively any predictive model (that the user can adapt
the formula input to accommodate). The example below is applied to the
`polr` function from the `{MASS}` package using `peformance::r2`’s
result as a returned value.

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

The call to `unlist` in the anonymous function above ensures that the
returned value is a numeric scalar as opposed to a list with a single
element.

### Random Forest

`domir` can also work with predictive models that do not produce model
coefficients like `randomForest::randomForest`. The dominance analysis
approach’s results differ from the built-in variable importance method
plotted below (which is arguably better suited for model selection) but
can, and in the case of many of the variables do, agree on which
independent variables are important.

The dominance analysis here is based on a squared correlation of the
predicted values with the dependent variable (i.e., the explained
variance $R^2$).

``` r
set.seed(5621)

rf_model <- 
  randomForest::randomForest(mpg ~ am + qsec + cyl, data = mtcars, 
                             importance = TRUE)

data.frame(`%IncMSE` = rf_model$importance[,1], `RankIncMSE` = rank(rf_model$importance[,1]*-1), `IncNodePurity` = rf_model$importance[,2], `RankIncPurity` = rank(rf_model$importance[,2]*-1),check.names = FALSE)
```

    ##        %IncMSE RankIncMSE IncNodePurity RankIncPurity
    ## am   10.121981          2      188.9050             3
    ## qsec  6.754203          3      281.3496             2
    ## cyl  20.526554          1      367.8784             1

``` r
cor(predict(rf_model), mtcars$mpg)^2
```

    ## [1] 0.7005082

``` r
domir(mpg ~ am + qsec + cyl, 
      \(fml) {
        set.seed(5621)
        result <- 
          randomForest::randomForest(fml, data = mtcars, 
                             importance = TRUE)
        cor <- cor(predict(result), mtcars$mpg)
        return(cor^2)
      }
)
```

    ## Overall Value:      0.7005082 
    ## 
    ## General Dominance Values:
    ##      General Dominance Standardized Ranks
    ## am           0.1600684    0.2285032     2
    ## qsec         0.1338248    0.1910396     3
    ## cyl          0.4066151    0.5804572     1
    ## 
    ## Conditional Dominance Values:
    ##      Subset Size: 1 Subset Size: 2 Subset Size: 3
    ## am        0.2642756      0.1741050    0.041824636
    ## qsec      0.2452030      0.1478614    0.008409932
    ## cyl       0.6761472      0.4206517    0.123046338
    ## 
    ## Complete Dominance Designations:
    ##              Dmnated?am Dmnated?qsec Dmnated?cyl
    ## Dmnates?am           NA         TRUE       FALSE
    ## Dmnates?qsec      FALSE           NA       FALSE
    ## Dmnates?cyl        TRUE         TRUE          NA

Note the use of `set.seed` prior to all calls to `randomForest`. These
ensure that the random processes within the `randomForest` function
result in a reproducible set of predicted values (and $R^2$ metric). The
calls to individual `randomForest`s also had to use the
`importance = TRUE` argument (though they are not used) to ensure
matching with the original result as they affect the state of the random
number generator.

### Zero-Inflated Poisson

One distinct advantage of having the level of flexibility in the
analytic pipeline that `domir` offers is that this that it can work
directly with modeling functions that are more complex. The example
below uses the `zeroinfl` model from the package `{pscl}` that accepts a
`Formula::Formula` object (i.e., a multi-equation formula) instead of a
standard R formula.

The below example uses the entries in the formula to plug into the
`Formula` object that will be submitted to the `zeroinfl` model.

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

In this example, note the absence of a dependent variable in the model
formula. `domir` does not require a left hand side/dependent variable to
accommodate situations like the one here where it is added later in the
analysis pipeline. Also, note that the *fml* passed to the pipeline is
repeated in the Poisson and inflation model equations and then adapted
to a `Formula` object before submitting to `zeroinfl`.

# Further Reading

*…to be populated…*
