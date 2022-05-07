## Minor version update: 0.3.2

This patch version update corrects the note from R-devel and R-patched checks on CRAN, along with a request from CRAN maintainers in the previous submission, related to: "Package suggested but not available for checking: 'dominanceanalysis'".

This patch version update also corrects warnings from R-devel checks from Linux platforms on CRAN related to: "domir-package.Rd:30: invalid email address: [jluchman\@gmail](mailto:jluchman@gmail){.email}\_com" (completed in 0.3.1).

Changes from version 0.3.0 also include:

-   Minor updates and editorial corrections to *Conceptual Introduction to Dominance Analysis* vignette

## Test environments

-   local pop!OS/ubuntu 21.10, R 4.2.0
-   Windows Server 2022, R-devel, 64 bit (rhub)
-   Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
-   Fedora Linux, R-devel, clang, gfortran (rhub)
-   win-builder (devel, release, and old-release)

## R CMD check results

Windows Server 2022, R-devel, 64 bit (rhub): 1 NOTE

    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'

Comment: Appears to be an issue with TeX on this platform. I cannot reproduce the error locally.

win-builder (devel, release, and old-release): 1 NOTE

    Found the following (possibly) invalid DOIs:
      DOI: 10.1037/1082-989X.8.2.129
        From: DESCRIPTION
        Status: Bad Request
        Message: 400
      DOI: 10.1198/000313007X188252
        From: DESCRIPTION
        Status: Service Unavailable
        Message: 503

Comment: DOI: 10.1037/1082-989X.8.2.129 links reliably to Azen and Budescu's 2003 article on dominance analysis when followed from a web browser (tested on Google Chrome and Mozilla Firefox).

Similarly, DOI: 10.1198/000313007X188252 links reliably to Groemping's 2007 article on relative importance in linear models when followed from a web browser (tested on Google Chrome and Mozilla Firefox).

## Downstream dependencies

There are no downstream dependencies for this package.
