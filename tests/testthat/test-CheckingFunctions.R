# Tests for check.evidence.levels ----------------------------------------------

test_that("check.evidence.levels passes with valid numeric input", {
  analyses_list <- list(list(analysis_parameters = list(quantiles = c(0.1, 0.2, 0.3))))
  expect_silent(check.evidence.levels(
    evidence_levels = c(0.8, 0.9),
    cohort_names = c("A", "B"),
    analyses_list = analyses_list,
    error_evidence_levels = "Invalid evidence levels"
  ))
})

test_that("check.evidence.levels fails when lengths differ", {
  analyses_list <- list(list(analysis_parameters = list(quantiles = c(0.1, 0.2))))
  expect_error(check.evidence.levels(
    evidence_levels = c(0.8),
    cohort_names = c("A", "B"),
    analyses_list = analyses_list,
    error_evidence_levels = "Invalid evidence levels"
  ), "must have the same length")
})

test_that("check.evidence.levels fails when character input has invalid strings", {
  analyses_list <- list(list(analysis_parameters = list(quantiles = c(0.1, 0.2))))
  expect_error(check.evidence.levels(
    evidence_levels = c("mean", "wrong"),
    cohort_names = c("A", "B"),
    analyses_list = analyses_list,
    error_evidence_levels = "Invalid evidence levels"
  ), "The only string allowed")
})

test_that("check.evidence.levels fails when numeric values out of [0,1]", {
  analyses_list <- list(list(analysis_parameters = list(quantiles = c(0.1, 0.2))))
  expect_error(check.evidence.levels(
    evidence_levels = c(-0.1, 1.2),
    cohort_names = c("A", "B"),
    analyses_list = analyses_list,
    error_evidence_levels = "Invalid evidence levels"
  ), "Invalid evidence levels")
})

test_that("check.evidence.levels fails when quantiles do not match", {
  analyses_list <- list(list(analysis_parameters = list(quantiles = c(0.1, 0.2))))
  expect_error(check.evidence.levels(
    evidence_levels = c(0.5, 0.6),
    cohort_names = c("A", "B"),
    analyses_list = analyses_list,
    error_evidence_levels = "Invalid evidence levels"
  ), "must have matches")
})

# Tests for checkForParallelBackend --------------------------------------------

test_that("checkForParallelBackend is silent when backend registered", {
  # Register a dummy backend
  doFuture::registerDoFuture()
  future::plan(future::sequential)
  expect_silent(checkForParallelBackend())
})

