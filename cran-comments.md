## Minor version update: 0.2.0
This minor version update has included new features to the `domin` function, added a unit testing framework, and a vignette introducing the concepts underlying the main function.
Changes from version 0.1.0 include:

* `consmodel` argument to adjust for baseline fit statistic values
* `reverse` argument to change rank and complete dominance interpretation for fit metrics that decrease with better fit
* *Conceptual Introduction to Dominance Analysis* vignette added
* implemented unit testing framework - covers 82% of `domin`
* internal use of `reformulate` to construct formulas as opposed to `paste`-ing `+`-es
* bugfix on`Complete_Dominance` computation producing inconsistent complete dominance designations - within-order complete dominance checks not capturing all possible model comparisons

## Test environments
* local pop!OS/ubuntu 21.04, R 4.1.1
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* win-builder (devel, release, and old-release)

## R CMD check results

One NOTE from Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub):

Found the following (possibly) invalid DOIs:
  DOI: 10.47263/JASEM.4(2)02

* This DOI is not invalid.  This paper is open-access and launches in a 
PDF viewer and not a web page.

## Downstream dependencies
There are no downstream dependencies for this package.
