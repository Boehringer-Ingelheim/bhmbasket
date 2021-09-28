## Test environments
- x86_64-w64-mingw32 (64-bit), R 4.0.5
- x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-09-09 r80873)
- R-hub ubuntu-gcc-devel (r-devel)
- R-hub macos-highsierra-release-cran (r-release)
- R-hub ubuntu-gcc-release (r-release)
- R-hub windows-x86_64-devel (r-devel)

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √

  Maintainer: 'Stephan Wojciekowski <stephan.wojciekowski@boehringer-ingelheim.com>'

## From NEWS.md
* Fixed a bug in continueRecruitment() that could result in an error message and prevent the function from running although the respective condition was met.

* Registration of parallel backend is now the responsibility of the user to allow for flexibility. A message has been added 

* Functions simulateScenarios() and performAnalyses() look up for their arguments 'n_trials' and 'n_mcmc_iterations', respectively, in the global environment

* Arguments 'n_subjects_list' and 'n_subjects_add_list' of the functions simulateScenarios() and continueRecruitment(), respectively, can be provided as a single vector for the case when all scenarios recruit the same number of subjects

* Added the argument 'overall_min_nogos' to the function negateGoDecisions()

* Added an input check in function getEstimates() regarding the argument 'add_parameters'

* Updated documentation

* Updated Imports and Suggests in DESCRIPTION

* Minor changes in code
