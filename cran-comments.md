## Resubmission: Major version update: 1.0.0

In this resubmission of major update 1.0.0 I have fixed a web link to a vignette in the README.md file at the request of CRAN maintainers.  This change has also removed one of the check NOTEs in the previous submission.

This major version update includes a new S3 generic function, `domir`, with a `print` and `summary` function and `formula` method.  This new generic function supersedes the `domin` function.

Changes from version 0.3.2 also include:

-   bug fix to `summary` function applied to `domin` for determining strongest dominance.

## Test environments

-   pop!OS/ubuntu 22.04, R 4.2.1 (local)
-   Apple Silicon (M1), macOS 11.6 Big Sur, R-release (rhub)
-   macOS 10.13.6 High Sierra, R-release, brew (rhub)
-   macOS 10.13.6 High Sierra, R-release, CRAN's setup (rhub)
-   Oracle Solaris 10, x86, 32 bit, R-release
-   Oracle Solaris 10, x86, 32 bit, R release, Oracle Developer Studio 12.6
-   win-builder (devel, release, and old-release)

## R CMD check results

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
