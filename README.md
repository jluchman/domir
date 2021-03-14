---
title: Tools to Support Relative Importance Analysis
output: github_document
---



# Overview

The `domir` package provides a set of flexible wrapper and helper functions for 
conducting relative importance analysis with a focus on dominance analysis. 
The intention of this package is to provide tools that allow relative 
importance analysis across a wide varitey of practical data analytic situations.

This documentation (for the time being) assumes some knowledge of the 
dominance analysis methodology and does not describe it extensively.

# Installation

To install the most recent stable version of `domir` from CRAN use:

`install.packages("domir")`

To install the working development version of `domir` using the `devtools` 
package use:

`devtools::install_github("https://github.com/jluchman/domir")`

# Details

The `domir` package is aimed at offering users tools for applying relative 
importnce/RI analysis to a wide variety of statistical or machine learning 
functions.  In addition, this package aims to allow users to apply new 
fit statistics or metrics to statistical or machine learning 
functions that already have RI analysis implementations such as `lm`.

Currently, the only RI tool implemented in `domir` is a dominance analysis 
method `domin`. `domin` is a flexible wrapper function that can be used with 
many modeling functions that accept a formula that follow the 
standard `response ~ terms` format. The format used by domin can be extended 
to other functions focused on indepedent variabe/predictor/feature-based RI 
can be accommodated with a additional wrapper functions based 
on the formula it creates and submits to modeling functions.  Some examples are
offered below.

# Examples

In this section, several examples of the `domin` function are outlined to 
illustrate how the function can accommodate a wide variety of different 
models--in some cases with a custom wrapper function.

## Linear Models with $R^2$

Fundamentally, `domir` is an extension of the `lmg` method in the 
`relaimpo` package and the example below is intended to illustrate how the 
differences between the packages by way of displayed information.


```r
library(domir)

domin(mpg ~ am + vs + cyl, "lm", list("summary", "r.squared"), data=mtcars)
```

```
## Overall Fit Statistic:      0.7619773 
## 
## General Dominance Statistics:
##     General_Dominance Standardized Ranks
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
## Complete Dominance Statistics:
##           Dmned?am Dmned?vs Dmned?cyl
## Dmate?am         0       -1        -1
## Dmate?vs         1        0        -1
## Dmate?cyl        1        1         0
```

```r
relaimpo::calc.relimp(lm(mpg ~ am + vs + cyl, data=mtcars))
```

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
```

## Linear Model with Alternative Fit Statstic

One, fairly basic, way that `domir` differs from other relative importance 
analysis modeling functions is in that it allows for different fit statistics 
other than the standard error variance $R^2$.  In this case, the McFadden 
pseudo-$R^2$ is used from the `pscl` package.


```r
domin(mpg ~ am + vs + cyl, "lm", list(function (model) 
    {pscl::pR2(model)},
        "McFadden"), data=mtcars)
```

```
## Overall Fit Statistic:      0.2243283 
## 
## General Dominance Statistics:
##     General_Dominance Standardized Ranks
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
## Complete Dominance Statistics:
##           Dmned?am Dmned?vs Dmned?cyl
## Dmate?am         0       -1        -1
## Dmate?vs         1        0        -1
## Dmate?cyl        1        1         0
```

## Ordered Logistic Regression

One analysis, to this point not supported in relative importance analysis 
software in R are models such as `polr` from the `MASS` package.


```r
mtcars2 <- data.frame(mtcars, carb2=as.factor(mtcars$carb))

domin(carb2 ~ am + vs + mpg, MASS::polr, 
    list(function(model) {pscl::pR2(model)}, "McFadden"), data=mtcars2)
```

```
## Overall Fit Statistic:      0.2647682 
## 
## General Dominance Statistics:
##     General_Dominance Standardized Ranks
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
## Complete Dominance Statistics:
##           Dmned?am Dmned?vs Dmned?mpg
## Dmate?am         0       -1        -1
## Dmate?vs         1        0         1
## Dmate?mpg        1       -1         0
```

## Multinomial Logistic (softmax) Regression

Another analysis, to this point not supported in relative importance analysis 
software in R are models such as `multinom` from the `nnet` package.


```r
domin(carb2 ~ am + vs + mpg, nnet::multinom, 
    list(function(model) {pscl::pR2(model)}, "McFadden"), data=mtcars2)
```

```
## Overall Fit Statistic:      0.4903441 
## 
## General Dominance Statistics:
##     General_Dominance Standardized Ranks
## am          0.1307969    0.2667452     3
## vs          0.1555933    0.3173146     2
## mpg         0.2039538    0.4159402     1
## 
## Conditional Dominance Statistics:
##         IVs: 1    IVs: 2    IVs: 3
## am  0.08335696 0.1246368 0.1843970
## vs  0.20850215 0.1494332 0.1088446
## mpg 0.20789005 0.1977937 0.2061777
## 
## Complete Dominance Statistics:
##           Dmned?am Dmned?vs Dmned?mpg
## Dmate?am         0       -1        -1
## Dmate?vs         1        0         1
## Dmate?mpg        1       -1         0
```

## Decision Trees

`domir` can also accept models that have traditionally never been dominance 
analyzed such as `rpart` models.

Note that the function accepted by `domin`s `fitstat` argument can be 
quite complex as it is a wrapper to `do.call`.  Note that anonymous 
functions defined in the first element of `fitstat` must return a named 
list with a name that is callable in the second entry of `fitstat`.


```r
mtcars2 <- data.frame(mtcars, carb2=as.factor(mtcars$carb))

domin(mpg ~ am + vs + cyl, rpart::rpart, 
    list(function(model) {list(R2=1-model$cptable[nrow(model$cptable),3])}, 
        "R2"), data=mtcars)
```

```
## Overall Fit Statistic:      0.7324601 
## 
## General Dominance Statistics:
##     General_Dominance Standardized Ranks
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
## Complete Dominance Statistics:
##           Dmned?am Dmned?vs Dmned?cyl
## Dmate?am         0       -1        -1
## Dmate?vs         1        0        -1
## Dmate?cyl        1        1         0
```

## Zero-Inflated Poisson

Complex models such as `zeroinfl` models from the package `pscl` can also 
be accommodated and as anonymous functions as well in `domin`s `reg` argument.

The below anonymous function in `reg` uses the entries in the formula to 
create a symmetric count and zero-inflation formulas.

The full model as submitted to `zeroinfl` looks like:

`art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment`


```r
domin(art ~ fem + mar + kid5 + phd + ment, 
        function(model, ...) {
            zip_terms <- paste(attr(terms(model), "term.labels"), collapse=" + ")
            zip_form <- paste(c(zip_terms, zip_terms), collapse=" | ")
            zip_form <- as.formula(paste(c(row.names(attr(terms(model), "factors"))[1], 
                zip_form), collapse=" ~ "))
            pscl::zeroinfl(zip_form, ...)
        },
    list(pscl::pR2, "McFadden"), data=pscl::bioChemists)
```

```
## Overall Fit Statistic:      0.04443172 
## 
## General Dominance Statistics:
##      General_Dominance Standardized Ranks
## fem        0.004833831   0.10879234     2
## mar        0.001092066   0.02457853     5
## kid5       0.002756213   0.06203257     3
## phd        0.001176447   0.02647763     4
## ment       0.034573161   0.77811893     1
## 
## Conditional Dominance Statistics:
##            IVs: 1       IVs: 2      IVs: 3       IVs: 4       IVs: 5
## fem  0.0054489923 0.0051709844 0.004844108 0.0045203889 4.184680e-03
## mar  0.0005852711 0.0008300824 0.001096062 0.0013714814 1.577435e-03
## kid5 0.0008854100 0.0017679158 0.002718124 0.0037198565 4.689761e-03
## phd  0.0023394473 0.0017607852 0.001169305 0.0005984014 1.429432e-05
## ment 0.0362830206 0.0353434159 0.034495496 0.0337386752 3.300520e-02
## 
## Complete Dominance Statistics:
##            Dmned?fem Dmned?mar Dmned?kid5 Dmned?phd Dmned?ment
## Dmate?fem          0         1          0         1         -1
## Dmate?mar         -1         0         -1        -1         -1
## Dmate?kid5         0         1          0         0          0
## Dmate?phd         -1         1          0         0         -1
## Dmate?ment         1         1          0         1          0
```

(more examples to come...)
