## Resubmission of patch version update: 1.0.1

This resubmission corrects the issue noted by the CRAN maintainers related to the /revdep/ directory and it's contents. I had neglected to add this directory to the .Rbuildignore. This has been fixed in the re-submission.

## Reiteration of CRAN Automatic Checks

    Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
    Check: top-level files, Result: NOTE
      Non-standard file/directory found at top level:
        'revdep'

    Flavor: r-devel-linux-x86_64-debian-gcc
    Check: if this is a source package, Result: NOTE
      Subdirectory 'revdep/library/domir/new/domir' seems to contain an installed version of the package.
      
Comment: This was an error of omission related to an unfortunate sequence of checks using the `{devtools}` software that I did not catch. This directory has now been added to .Rbuildignore and passes all checks.

## Additional Release Notes

This patch update to 1.0.0 has improved the consistency of argument checks 
and has disallowed the use of `offset()`s in formulas with `domin` and `domir` 
functions--with a recommendation the user incorporate them in the functions 
called by both when necessary.

## Test environments

-   pop!OS/ubuntu 22.04, R 4.3.0 (local)
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

Windows Server 2022, R-devel, 64 bit: 2 NOTES

    * checking HTML version of manual ... NOTE
    Skipping checking math rendering: package 'V8' unavailable
 
Comment: Assume the note stems from a temporary issue with package `{V8}` on the RHub platform and is not reproducible in many other platforms.
    
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
      
Comment: Appears to be a Windows LaTeX/MikTek cleanup issue and does not appear/is not reproducible in win-builder.

Fedora Linux, R-devel, clang, gfortran: 1 NOTE

    * checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found
    Skipping checking math rendering: package 'V8' unavailable
    
Comment: Assume the note stems from a temporary issue with package `{V8}` and `{broom}` on the RHub platform and is not reproducible in many other platforms.

Ubuntu Linux 20.04.1 LTS, R-release, GCC: 1 NOTE

    Found the following (possibly) invalid DOIs:
      DOI: 10.1002/asmb.446
        From: DESCRIPTION
        Status: Forbidden
        Message: 403
      DOI: 10.1037/1082-989X.8.2.129
        From: DESCRIPTION
        Status: Forbidden
        Message: 403

Comment: DOI: 10.1002/asmb.446 links reliably to Lipovetsky's and Conklin's 2001 article on relative importance in linear models when followed from a web browser (tested on Mozilla Firefox). In addition, DOI: links reliably to Azen and Budescu's 2003 article on dominance analysis when followed from a web browser (again tested on Mozilla Firefox).

## Downstream dependencies

There are no downstream dependencies for this package on CRAN.
