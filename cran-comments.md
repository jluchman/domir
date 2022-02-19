## Minor version update: 0.3.0
This minor version update has included new features and updates to internal computation methods for the `domin` function, a `summary` function that can be called with a `domin` class object, and extensive updates to the vignette introducing the concepts underlying the main function.

Changes from version 0.2.0 include:

* `conditional` logical argument to make computing conditional dominance statistics optional
* initial working version of `summary.domin` 
  * method for computing the strongest dominance designations between independent variables
  * update to `print.domin` to display strongest dominance designations
* reorganization and update to *Conceptual Introduction to Dominance Analysis* vignette
* looping used to construct many dominance statistics, replaced with matrix-based methods; intended to simplify and expedite computations

## Test environments
* local pop!OS/ubuntu 21.10, R 4.1.2
* Windows Server 2022, R-devel, 64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* win-builder (devel, release, and old-release)

## R CMD check results

All tests: 1 NOTE

Found the following (possibly) invalid DOIs:
  DOI: 10.1037/1082-989X.8.2.129
    
Comment: This DOI is not invalid and reliably links to Budescu's 1993 article on dominance analysis.

## Downstream dependencies
There are no downstream dependencies for this package.
