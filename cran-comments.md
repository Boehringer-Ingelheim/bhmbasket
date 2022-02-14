Maintainer: 'Stephan Wojciekowski <stephan.wojciekowski@boehringer-ingelheim.com>'

## Test environments
- x86_64-w64-mingw32 (64-bit), R 4.1.2
- x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2022-02-13 r81727 ucrt)
- R-hub ubuntu-gcc-release (r-release)
- R-hub ubuntu-gcc-devel (r-devel)
- R-hub macos-m1-bigsur-release (r-release)
- R-hub macos-highsierra-release-cran (r-release)

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

=> This DOI has not been changed since since bhmbasket version 0.9.1 and the respective publication can be accessed using this DOI via https://www.doi.org/

### R-hub ubuntu-gcc-release (r-release)
* checking CRAN incoming feasibility ... NOTE

Found the following (possibly) invalid DOIs:
  DOI: 10.1177/1740774513497539
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503

=> This DOI has not been changed since bhmbasket version 0.9.1 and the respective publication can be accessed using this DOI via https://www.doi.org/

### R-hub macos-m1-bigsur-release (r-release)
OK

### R-hub macos-highsierra-release-cran (r-release)
OK

## From NEWS.md

### Fixed Bugs

* Fixed a bug that occured in performAnalyses() using R-devel due to a recent change in stats::aggregate()

### New & Altered Features

* Introduced nested parallelization for better usage of HPC resources

* Introduced chunking of tasks for better performance in parallel environments

* Updated vignette on HPC environment

* Update documentation of performAnalyses()

* Recommended doFuture and future over doParallel and parallel when no parallel backend is detected

* Updated Suggests and SystemRequirements in DESCRIPTION

* Added a WORDLIST

* Minor changes in code