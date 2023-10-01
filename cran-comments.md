## Patch version update: 1.1.1

This patch update fixed a bug in the computation of a returned matrix when an 
infrequently used optional argument is non-`NULL` and added the *lme4* package 
to this package's list of suggested packages given its reference in 
documentation and a warning from CRAN checks.

## CRAN check results

    Check: Rd cross-references
    Result: NOTE
        Undeclared package ‘lme4’ in Rd xrefs
    Flavor: r-devel-linux-x86_64-fedora-clang
    
Comment: `lme4`has been added to this package's list of suggested packages.

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
 
Comment: Previously, this issue arose with the package `{V8}` on the RHub platform and was not reproducible on the win-builder, macOS-builder, or my local checks.

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
    
Comment: This is a similar issue to Rhub's Windows check in that the `{V8}` package seems unavailable and is throwing a NOTE. Again, not an issue that occured with win-builder, macOS-builder, or my local checks.

Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub): 1 NOTE

    * checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found
    Skipping checking math rendering: package 'V8' unavailable
    
Comment: Consistent with the Fedora-based check, the Ubuntu check is throwing a NOTE given the `{V8}` package is seems unavailable. Again, not an issue that occured with win-builder, macOS-builder, or my local checks.

## Downstream dependencies

There are no downstream dependencies for this package on CRAN.
