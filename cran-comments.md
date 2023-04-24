## Patch version update: 1.0.1

This patch update to 1.0.0 has improved the consistency of argument checks 
and has disallowed the use of `offset()`s in formulas with `domin` and `domir` 
functions--with a recommendation the user incorporate them in the functions 
called by both when necessary.

## Test environments

-   pop!OS/ubuntu 22.04, R 4.2.3 (local)
-   Windows Server 2022, R-devel, 64 bit (rhub)
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

Windows Server 2022, R-devel, 64 bit: 2 NOTES

    * checking HTML version of manual ... NOTE
    Skipping checking math rendering: package 'V8' unavailable
 
Comment: Assume the note stems from a temporary issue with package `{V8}` on the RHub platform and is not reproducible in other platforms.
    
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
      
Comment: Appears to be a Windows LaTeX/MikTek cleanup issue and does not appear/is not reproducible in win-builder.

## Downstream dependencies

There are no downstream dependencies for this package on CRAN.
