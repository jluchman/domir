## Minor version update: 1.1.0

This minor update has added a new S3 method the the primary `domir()` function
that parses a list of formulas by RHS~LHS name pairs. This update has also
begun an extensive code linting process to improve its readability and 
complexity.

## Test environments

-   pop!OS/ubuntu 22.04, R 4.3.1 (local)
-   Windows Server 2022, R-devel, 64 bit (rhub)
-   Fedora Linux, R-devel, clang, gfortran (rhub)
-   Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
-   win-builder (devel, release, and old-release)
-   macOS-builder (release)

## R CMD check results

win-builder-old-release: 1 NOTE

    Found the following (possibly) invalid DOIs:
    DOI: 10.1002/asmb.446
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
      
Comment: DOI: 10.1002/asmb.446 links reliably to Lipovetsky's and Conklin's 2001 article on relative importance in linear models when followed from a web browser (tested on Mozilla Firefox).

Windows Server 2022, R-devel, 64 bit: 3 NOTES

    * checking HTML version of manual ... NOTE
    Skipping checking math rendering: package 'V8' unavailable
 
Comment: Assume the note stems from a temporary issue with package `{V8}` on the RHub platform and is not reproducible in many other platforms.

    * checking for non-standard things in the check directory ... NOTE
    Found the following files/directories:
    ''NULL''

Comment: Commentary on StackOverflow suggests this is a problem with loading one or more packages on the RHub platform for the Windows server.
    
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
      
Comment: Appears to be a Windows LaTeX/MikTek cleanup issue and does not appear/is not reproducible in win-builder.

Fedora Linux, R-devel, clang, gfortran and Ubuntu Linux 20.04.1 LTS, R-release, GCC: 1 NOTE

    * checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found
    Skipping checking math rendering: package 'V8' unavailable
    
Comment: Assume the note stems from an issue with package `{V8}` and `{broom}` on the RHub platform and is not reproducible in many other platforms.

## Downstream dependencies

There are no downstream dependencies for this package on CRAN.
