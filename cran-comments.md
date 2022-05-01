## Minor version update: 0.3.1

This patch version update corrects warnings from R-devel checks from Linux platforms on CRAN related to: "domir-package.Rd:30: invalid email address: [jluchman\@gmail](mailto:jluchman@gmail){.email}\_com".

Changes from version 0.3.0 also include:

-   Minor updates and editorial corrections to *Conceptual Introduction to Dominance Analysis* vignette

## Test environments

-   local pop!OS/ubuntu 21.10, R 4.2.0
-   Windows Server 2022, R-devel, 64 bit (rhub)
-   Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
-   Fedora Linux, R-devel, clang, gfortran (rhub)
-   win-builder (devel, release, and old-release)

## R CMD check results

Fedora Linux, R-devel, clang, gfortran (rhub): 1 ERROR

    checking package dependencies ... ERROR Packages suggested but not available: 'relaimpo', 'tidyverse'

    The suggested packages are required for a complete check. Checking can be attempted without them by setting the environment variable *R_CHECK_FORCE_SUGGESTS* to a false value.

    See section 'The DESCRIPTION file' in the 'Writing R Extensions' manual.

Comment: CRAN checks last updated on 2022-05-01 16:52:10 CEST for r-devel-linux-x86_64-debian-gcc for `relaimpo`, a suggested package in `domir`, show dependency `survey` is not available. Similarly, CRAN checks last updated on 2022-05-01 16:52:10 CEST for r-devel-linux-x86_64-debian-gcc for `tidyverse`, also a suggested package in `domir`, show dependencies `haven` and `readr` is not available. Given the nature of the error, I do not believe the problem stems from the `domir` package.

Windows Server 2022, R-devel, 64 bit (rhub): 1 NOTE

    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'

Comment: Appears to be an issue with TeX on this platform. I cannot reproduce the error locally.

win-builder (devel, release, and old-release): 1 NOTE

    Found the following (possibly) invalid DOIs: DOI: 10.1037/1082-989X.8.2.129

Comment: This DOI is not invalid and reliably links to Budescu's 1993 article on dominance analysis.

## Downstream dependencies

There are no downstream dependencies for this package.
