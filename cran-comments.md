## Patch version update: 1.0.1

This patch update to 1.0.0 has improved the consistency of argument checks 
and has disallowed the use of `offset()`s in formulas with `domin` and `domir` 
functions--with a recommendation the user incorporate them in the functions 
called by both when necessary.

## Test environments

-   pop!OS/ubuntu 22.04, R 4.2.3 (local)
# -   Apple Silicon (M1), macOS 11.6 Big Sur, R-release (rhub)
# -   macOS 10.13.6 High Sierra, R-release, brew (rhub)
# -   macOS 10.13.6 High Sierra, R-release, CRAN's setup (rhub)
-   win-builder (devel, release, and old-release)
-   macOS-builder (release)

## R CMD check results

All checks on all platforms: OK.

win-builder-old-release: 1 NOTE

    Found the following (possibly) invalid DOIs:
    DOI: 10.1002/asmb.446
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
      
Comment: DOI: 10.1002/asmb.446 links reliably to Lipovetsky's and Conklin's 2001 article on relative importance in linear models when followed from a web browser https://doi.org (tested on Mozilla Firefox).

...

Oracle Solaris with and without Oracle Developer Studio (rhub): 1 ERROR

    * checking package dependencies ... ERROR
      Package suggested but not available: ‘testthat’

      The suggested packages are required for a complete check.
      Checking can be attempted without them by setting the environment
      variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

      See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
      manual.

Comment: Appears to be a platform-specific build issue where the 'testthat' package relied on for unit testing was not available resulting in a failed check on dependencies.

win-builder (devel, release, and old-release) and Oracle Solaris with and without Oracle Developer Studio (rhub): 1 NOTE

    Possibly misspelled words in DESCRIPTION:
      Conklin (17:22)
      Lipovetsky (17:4)
      Shapley (16:38)
      Gr�mping (15:36)

Comment: Each word flagged as misspelled is an authors' name from cited literature discussing this methodology.

    Found the following (possibly) invalid DOIs:
      DOI: 10.1002/asmb.446
        From: DESCRIPTION
        Status: Service Unavailable
        Message: 503
      DOI: 10.1037/1082-989X.8.2.129
        From: DESCRIPTION
        Status: Bad Request
        Message: 400

Comment: DOI: 10.1037/1082-989X.8.2.129 links reliably to Azen and Budescu's 2003 article on dominance analysis when followed from a web browser using https://doi.org (tested on Mozilla Firefox).

Similarly, DOI: 10.1002/asmb.446 links reliably to Lipovetsky's and Conklin's 2001 article on relative importance in linear models when followed from a web browser https://doi.org (tested on Mozilla Firefox).

## Downstream dependencies

There are no downstream dependencies for this package on CRAN.
