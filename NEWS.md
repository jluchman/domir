---
title: domir NEWS
---
# domir 0.2.0

- visible
  - `consmodel` argument to adjust for baseline fit statistic values
  - `reverse` argument to change rank and complete dominance interpretation for fit metrics that decrease with better fit
  - *Conceptual Introduction to Dominance Analysis* vignette added
- internal 
  - implemented unit testing framework - covers 82% of `domin`
  - internal use of `reformulate` to construct formulas as opposed to `paste`-ing `+`-es
- bug fixes 
  - `Complete_Dominance` computation producing inconsistent complete dominance designations - within-order complete dominance checks not capturing all possible model comparisons

# domir 0.1.0

- visible
  - update to format of `Complete_Dominance` from integers to logicals
  - output and warnings no longer suppressed by default - user must silence noisy functions
  - minimum of two terms/sets to run `domin` (replicates behavior of Stata version)
  - additional checks (response in formula/orders in formula)
  - names of entries in `domin` object changed to syntactic R names when not.  Affects entries in:
    - `Complete_Dominance` matrix
    - `Conditional_Dominance` matrix
-internal
  - extensive re-write of computation methods
    - pre-allocates container objects to improve performance
    - increased functional-ization of subroutines and overall code readability
    - many internal functions re-named
- bug fixes 
  - returned value .$Subset_Details$Full_Model now includes variables in `all`
  - `Complete_Dominance` matrix computation
    - Too many models considered for complete dominance - inconsistent with standard dominance analysis methodology
    - Fixed error in suppressing complete dominance (i.e., `complete = FALSE`) resulted in error 

# domir 0.0.1

- bug fixes 
  - `Ensemble_Coordinator` that submitted factor #'s instead of text in R versions < 4

# domir 0.0.0

- initial working version of `domin`
- initial working version of `print.domin`
