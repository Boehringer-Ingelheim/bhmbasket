seed <- 1633099466 #as.integer(Sys.time())
set.seed(seed)

doFuture::registerDoFuture()
future::plan(future::multisession)

scenarios <- bhmbasket::simulateScenarios(
  n_subjects_list     = list(c(15, 15, 20),
                             c(15, 10, 20)),
  response_rates_list = list(c(0.3, 0.4, 6),
                             c(0.1, 0.2, 6)),
  n_trials            = 1e4)
# 
# scenarios <- bhmbasket::simulateScenarios(
#   n_subjects_list     = c(15, 15, 20),
#   response_rates_list = list(c(0.3, 0.4, 6),
#                              c(0.1, 0.2, 6)),
#   n_trials            = 1e4)

# analyses <- bhmbasket::performAnalyses(
#   scenario_list    = scenarios,
#   method_names     = c("exnex_adj", "pooled", "stratified"),
#   calc_differences = matrix(c(1, 2, 3, 3), ncol = 2),
#   target_rates     = c(0.25, 0.45, 0.3))

analyses <- bhmbasket::performAnalyses(
  scenario_list    = scenarios,
  method_names     = c("berry", "pooled", "stratified"),
  calc_differences = c(2, 3),
  target_rates     = c(0.1, 0.1, 0.1))

bhmbasket::getEstimates(analyses,
                        add_parameters = c("mu"))

go_decisions <- bhmbasket::getGoDecisions(
  analyses_list   = analyses,
  cohort_names    = c("p_1", "p_2", "p_diff_23"),
  evidence_levels = c(0.5, 0.95, 0.5),
  boundary_rules  = quote(c(x[1] > 0.2, x[2] > 0.3 && x[3] > 0,
                            FALSE)),
  overall_min_gos = 1)

bhmbasket::getGoProbabilities(go_decisions)

updated_scenarios <- bhmbasket::continueRecruitment(
  n_subjects_add_list = list(c(10, 10),
                             c(10, 10)),
  decisions_list      = go_decisions,
  method_name         = "berry")

updated_analyses <- bhmbasket::performAnalyses(
  scenario_list    = updated_scenarios,
  calc_differences = c(3, 1),
  method_names     = c("berry", "pooled", "stratified"),
  target_rates     = c(0.1, 0.1, 0.1))

bhmbasket::getEstimates(updated_analyses)

updated_go_decisions <- bhmbasket::getGoDecisions(
  analyses_list   = updated_analyses,
  cohort_names    = c("p_1", "p_2", "p_3",
                      "p_1", "p_2", "p_3"),
  evidence_levels = c(0.5,  0.5,  0.5,
                      0.95, 0.95, 0.95),
  boundary_rules  = quote(c(x[1] > 0.5 & x[4] > 0.3,
                            x[2] > 0.5 & x[5] > 0.3,
                            x[3] > 0.5 & x[6] > 0.3)))

updated_nogo_decisions <- bhmbasket::getGoDecisions(
  analyses_list   = updated_analyses,
  cohort_names    = c("p_1", "p_2", "p_3",
                      "p_1", "p_2", "p_3"),
  evidence_levels = c(0.5,  0.5,  0.5,
                      0.95, 0.95, 0.95),
  boundary_rules  = quote(c(x[1] > 0.5 | x[4] > 0.3,
                            x[2] > 0.5 | x[5] > 0.3,
                            x[3] > 0.5 | x[6] > 0.3)))
updated_nogo_decisions <- bhmbasket::negateGoDecisions(updated_nogo_decisions)

probs <- bhmbasket::getGoProbabilities(updated_go_decisions, updated_nogo_decisions)

bhmbasket::scaleRoundList(probs, 100, 2)



borrowing_scenario <- bhmbasket::simulateScenarios(
  n_subjects_list     = list(c(20, 20)),
  response_rates_list = list(c(0.1, 0.9)),
  n_trials            = 5e4)

borrowing_analyses <- bhmbasket::performAnalyses(
  scenario_list     = borrowing_scenario,
  method_names      = c("exnex", "exnex_adj"),
  target_rates      = c(0.1, 0.9),
  n_mcmc_iterations = 1e5)

borrowing_estimates <- bhmbasket::getEstimates(
  borrowing_analyses,
  add_parameters = c("p_1", "p_2", "w_1", "w_2"))

borrowing_estimates <- bhmbasket::getEstimates(
  borrowing_analyses,
  add_parameters = c("w_1", "w_2"))

bhmbasket::scaleRoundList(borrowing_estimates, 100, 2)

