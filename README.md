# User-definable Dominance Analysis
## domir: 'dom'inance analysis 'i'n 'r'

The `domir` package offers users a way to apply dominance analysis to any 
statistical or machine learning functions that can 

Currently, only the `domin` function is operational.  This function requires 
the statistical or machine learning function to be dominance analyzed accept 
a `formula` object with a single response and all additive terms.

``` r
domin(function=mpg ~ am + vs + cyl, reg="lm", fitstat=list("summary", "r.squared"), data=mtcars)

Dominance Analysis with lm and summary in element r.squared 
List of 8
 $ General_Dominance        : num [1:3] 0.177 0.203 0.382
 $ Standardized             : num [1:3] 0.233 0.266 0.501
 $ Ranks                    : num [1:3] 3 2 1
 $ Conditional_Dominance    : num [1:3, 1:3] 0.36 0.441 0.726 0.139 0.164 ...
 $ Complete_Dominance       : num [1:3, 1:3] 0 1 1 -1 0 1 -1 -1 0
 $ Fit_Statistic_Overall    : num 0.762
 $ Fit_Statistic_All_Subsets: NULL
 $ Model_Details            :List of 2
  ..$ reg    : chr "lm"
  ..$ fitstat:List of 2
  .. ..$ function: chr "summary"
  .. ..$ element : chr "r.squared"
 - attr(*, "class")= chr [1:2] "domin" "list"
```
