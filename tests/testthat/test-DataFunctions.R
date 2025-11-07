test_that("simulateScenarios has correct structure", {
  
  #example case
  n_subjects <- c(10, 20, 30)
  rr_1 <- c(0.1, 0.1, 0.1)
  rr_2 <- c(0.9, 0.9, 0.9)
  
  scenarios <- simulateScenarios(
    n_subject_list = list(n_subjects,n_subjects),
    response_rates_list = list(rr_1, rr_2),
    scenario_numbers = c(1, 2),
    n_trials = 25
  )
  
  expect_s3_class(scenarios, "scenario_list")
  expect_equal(names(scenarios), c("scenario_1", "scenario_2"))
  
  s1 <- scenarios[[1]]
  
  #structure
  expect_true(is.matrix(s1$n_subjects))
  expect_true(is.matrix(s1$response_rates))
  
  expect_equal(s1$n_trails, 25)
  expect_equal(ncol(s1$n_subjects), length(n_subjects))
  
})



test_that("simulateScenarios has no mismatch and gives appropriate error messages", {
  
  #example case
  n_subjects <- list(c(10, 20, 30))
  rr_1 <- c(0.1, 0.1, 0.1)
  
  expect_error(simulateScenarios(n_subjects_list = n_subjects)
    
  expect_error(simulateScenarios(response_rates_list = rr_1)
  
  expect_error(simulateScenarios
               
  expect_error(simulateScenarios(list(n_subjects), list(rr_1, rr_1)))
  
  expect_error(simulateScenarios
  
  expect_error(simulateScenarios(

}