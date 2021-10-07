# bhmbasket 0.9.3

## Fixed Bugs

* Fixed a bug in continueRecruitment() that could result in an error message and prevent the function from running although all conditions were met.

* Fixed a bug leading to an error messages in getEstimates(). It would occur if in a previous call of performAnalyses() differences between cohorts were calculated, but not in the current call of performAnalyses().

## New & Altered Features

* Registration of parallel backend is now the responsibility of the user to allow for flexibility. A message has been added 

* Functions simulateScenarios() and performAnalyses() look up for their arguments 'n_trials' and 'n_mcmc_iterations', respectively, in the global environment

* Arguments 'n_subjects_list' and 'n_subjects_add_list' of the functions simulateScenarios() and continueRecruitment(), respectively, can be provided as a single vector for the case when all scenarios recruit the same number of subjects

* Added the argument 'overall_min_nogos' to the function negateGoDecisions()

* Added an input check in function getEstimates() regarding the argument 'add_parameters'

* Updated documentation

* Updated Imports and Suggests in DESCRIPTION

* Minor changes in code

# bhmbasket 0.9.2

* Fixed a rare bug that could result in wrong overall decision in getGoDecisions() in the presence of previous go decisions

* Updated documentation

* Updated Description in DESCRIPTION

* Minor changes in code

# bhmbasket 0.9.1

* Measures to fulfill CRAN policies and recommendations

# bhmbasket 0.9.0

* Initial release
