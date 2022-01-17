Maintainer: 'Stephan Wojciekowski <stephan.wojciekowski@boehringer-ingelheim.com>'

## Test environments
- x86_64-w64-mingw32 (64-bit), R 4.1.2
- x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2022-01-16 r81507 ucrt)
- R-hub ubuntu-gcc-devel (r-devel)
- R-hub macos-m1-bigsur-release (r-release)
- R-hub ubuntu-gcc-release (r-release)
- R-hub windows-x86_64-devel (r-devel)

## R CMD check results

### Local
0 errors √ | 0 warnings √ | 0 notes √

### R Winbuilder
Status: OK

### R-hub ubuntu-gcc-devel (r-devel)
* checking CRAN incoming feasibility ... NOTE

Found the following (possibly) invalid DOIs:
  DOI: 10.1177/1740774513497539
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503

=> This DOI has not been changed since the last update and the respective publication can be accessed using this DOI via https://www.doi.org/

### R-hub macos-m1-bigsur-release (r-release)
OK

### R-hub ubuntu-gcc-release (r-release)
* checking CRAN incoming feasibility ... NOTE

Found the following (possibly) invalid DOIs:
  DOI: 10.1177/1740774513497539
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503

=> This DOI has not been changed since the last update and the respective publication can be accessed using this DOI via https://www.doi.org/

### R-hub windows-x86_64-devel (r-devel)
* checking CRAN incoming feasibility ... NOTE

Found the following (possibly) invalid DOIs:
    From: DESCRIPTION
  DOI: 10.1177/1740774513497539
    Status: Service Unavailable
    Message: 503
    
=> This DOI has not been changed since the last update and the respective publication can be accessed using this DOI via https://www.doi.org/
    
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

=> This note only appears on this server and I do not think that it is caused by the R package. I suspect a server issue

## From NEWS.md

### Fixed Bugs

* Fixed a bug in continueRecruitment() that could result in additional subjects to be recruited when the overall decision for a trial realization is NoGo but some cohorts of that trial realization have Go decisions.

* Fixed a bug in performAnalyses() that would occur if all trial realizations of a scenario had a previous overall NoGo decision and would result in performAnalyses() to return an empty list for that scenario's posterior quantiles.

* Specified R2jags package version requirement in DESCRIPTION to prevent 'unused argument' bug in performAnalyses()

* Fixed warning message not showing when specifying deprecated arguments 'seed' and 'n_cores' in performAnalyses() 

### New & Altered Features

* Usage of doRNG package for fully reproducible results in parallel execution

* Usage of hash tables for mapping unique trial realizations to scenario trial realizations resulting in performance improvement

* Added a vignette that provides a short example on how to use bhmbasket in a high performance computing environment

* JAGS model files stored in package instead of writing to temporary files

* Updated Imports in DESCRIPTION

* Updated CITATION

* Minor changes in code

* Updated documentation

* Removed superfluous files