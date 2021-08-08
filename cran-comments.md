## Minor version update: 0.1.0
This minor version update has updates to the function mechanics underling 
the `domin` function, extensive additional documentation, and bug fixes.
Changes from version 0.1.0 include:

* bug fixes to `Complete_Dominance` matrix computation
* update to format of `Complete_Dominance` from integers to logicals
* extensive re-write of computation methods
* output and warnings no longer suppressed by default - user must silence noisy functions
* minimum of two terms/sets to run `domin` (replicates behavior of Stata version)
* additional checks (response in formula/orders in formula)
* names of entries in `domin` object changed to syntactic R names when not.  Affects entries in:
* returned value .$Subset_Details$Full_Model now includes variables in `all`

## Test environments
* local pop!OS/ubuntu 21.04, R 4.1.0
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
