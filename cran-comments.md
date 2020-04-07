## Test environments

* local OS X install, R 3.6.1
* Ubuntu 14.04 (on travis-ci), R-oldrel, R-release, R-devel
* Windows Server 2012 R2 (x64), R 3.6.1
* Rhub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Debian Linux, R-devel, GCC ASAN/UBSAN
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## Warning on Fedora Linux (Rhub)
  * 'framed.sty' not found
  * This package 'framed.sty' is required by knitr and seems to be lacking on that server.

## Notes on Fedora Linux (Rhub)
  * optimalThreshold took 5.802 seconds, but less than 5 sec on other servers.
  * Maintainer: ‘Murat Sariyar <murat.sariyar@bfh.ch>’, which is due to a switch from first author to the second one.
  * Package was archived on CRAN, as the first and second authors of the package were not availabe for a period of time.
  * Possibly mis-spelled words in DESCRIPTION: deduplicating (7:49), which is not a mispelling. 
  
## Notes on Windows Server 2008 (Rhub)
  * Unable to find GhostScript executable to run checks on size reduction (it seems that GS is not installed on that server)
  * non-standard things in the check directory ...  'RecordLinkage-Ex_i386.Rout' 'RecordLinkage-Ex_x64.Rout' 'examples_i386' 'examples_x64' (just the output of the vignette examples)
