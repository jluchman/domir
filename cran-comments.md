## Patched version: 0.0.1
This patched version has a major bug fix and 
additional documentation.  Changes from version 0.0.0 include:

* `Ensemble_Coordinator` submits `utils::combn`'s columns instead of coercing 
to `data.frame` and assuming it will be a `character` variable. Produced 
errors in R versions < 4.
** fix to version 0.0.0 ERROR on CRAN checks: 
*** r-oldrel-macos-x86_64
*** r-oldrel-windows-ix86+x86_64
* README.[R]md included and updated with multiple examples.
* NEWS.md included with updates.
* Updated DESCRIPTION file.
** fix to version 0.0.0 NOTE related to `datasets` import on for CRAN checks: 
*** r-devel-linux-x86_64-fedora-clang
*** r-devel-linux-x86_64-fedora-gcc
*** r-devel-windows-x86_64-gcc10-UCRT
*** r-patched-solaris-x86
*** r-release-macos-x86_64

## Test environments
* local pop!OS/ubuntu 20.10, R 4.0.4
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
