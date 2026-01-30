applicablePreviousTrialsNormal <- function(
    scenario_list,
    quantiles,
    n_cohorts,
    calc_differences
) {
  has_prev <- all(sapply(scenario_list, function(x) {
    !is.null(x$previous_analyses$post_quantiles)
  }))
  
  if (!has_prev) return(FALSE)
  
  ref_pq <- scenario_list[[1]]$previous_analyses$post_quantiles
  if (is.null(ref_pq$normal)) return(FALSE)
  
  ref_mat <- ref_pq$normal[[1]]
  
  same_structure <- all(sapply(seq_along(scenario_list), function(i) {
    pq <- scenario_list[[i]]$previous_analyses$post_quantiles
    if (is.null(pq$normal)) return(FALSE)
    mat <- pq$normal[[1]]
    identical(rownames(mat), rownames(ref_mat)) &&
      identical(colnames(mat), colnames(ref_mat))
  }))
  
  if (!same_structure) return(FALSE)
  
  needed_q_names <- paste0(quantiles * 100, "%")
  stored_q_names <- rownames(ref_mat)
  cond_q_subset  <- all(needed_q_names %in% stored_q_names)
  
  needed_cols <- paste0("mu_", seq_len(n_cohorts))
  cond_cols   <- all(needed_cols %in% colnames(ref_mat))
  
  if (!is.null(calc_differences)) {
    diff_names <- apply(calc_differences, 1, function(x) {
      paste0("mu_diff_", paste0(as.character(x), collapse = ""))
    })
    cond_diffs <- all(diff_names %in% colnames(ref_mat))
  } else {
    cond_diffs <- TRUE
  }
  
  has_prev && same_structure && cond_q_subset && cond_cols && cond_diffs
}

calcDiffsMCMCNormal <- function(posterior_samples, calc_differences) {
  calcDiffsMCMC(posterior_samples = posterior_samples,
                calc_differences  = calc_differences)
}

getModelFileNormal <- function() {
  system.file(
    package  = "bhmbasket",
    "jags_models",
    "bhm_normal.txt",
    mustWork = TRUE
  )
}

getPosteriorsNormal <- function(
    j_parameters,
    j_model_file,
    j_data,
    n_mcmc_iterations
) {
  getPosteriors(
    j_parameters      = j_parameters,
    j_model_file      = j_model_file,
    j_data            = j_data,
    n_mcmc_iterations = n_mcmc_iterations
  )
}

getPostQuantilesNormal <- function(
    method_name,
    quantiles,
    scenario_data,
    calc_differences = NULL,
    j_parameters,
    j_model_file,
    j_data,
    n_mcmc_iterations = 1e4,
    save_path         = NULL,
    save_trial        = NULL
) {
  if (!identical(method_name, "bhm_normal")) {
    stop("getPostQuantilesNormal currently only supports method_name = 'bhm_normal'")
  }
  
  trials   <- scenario_data$trials
  n_trials <- length(trials)
  
  if (is.null(save_trial) && !is.null(save_path)) {
    save_trial <- sample(seq_len(n_trials), size = 1)
  }
  
  out <- vector("list", n_trials)
  
  for (t in seq_len(n_trials)) {
    y_list <- trials[[t]]$y_list
    out[[t]] <- getPostQuantilesOfTrialNormal(
      y_list           = y_list,
      j_data           = j_data,
      j_parameters     = j_parameters,
      j_model_file     = j_model_file,
      method_name      = method_name,
      quantiles        = quantiles,
      calc_differences = calc_differences,
      n_mcmc_iterations = n_mcmc_iterations,
      save_path        = save_path,
      save_trial       = if (!is.null(save_trial) && t == save_trial) t else NULL
    )
  }
  
  out
}

getPostQuantilesOfTrialNormal <- function(
    y_list,
    j_data,
    j_parameters,
    j_model_file,
    method_name,
    quantiles,
    calc_differences,
    n_mcmc_iterations,
    save_path,
    save_trial
) {
  K <- length(y_list)
  if (K < 1L) stop("y_list must be a non-empty list of numeric vectors.")
  
  n_vec <- vapply(y_list, length, integer(1L))
  if (any(n_vec <= 0L)) stop("All cohorts in y_list must have at least one observation.")
  
  max_n <- max(n_vec)
  y_mat <- matrix(NA_real_, nrow = K, ncol = max_n)
  for (k in seq_len(K)) {
    y_mat[k, seq_len(n_vec[k])] <- y_list[[k]]
  }
  
  j_data_full <- c(
    j_data,
    list(
      K = K,
      n = n_vec,
      y = y_mat
    )
  )
  
  posterior_samples <- getPosteriorsNormal(
    j_parameters      = j_parameters,
    j_model_file      = j_model_file,
    j_data            = j_data_full,
    n_mcmc_iterations = n_mcmc_iterations
  )
  
  mu_cols <- grepl("^mu_[0-9]+$", colnames(posterior_samples))
  mu_post <- posterior_samples[, mu_cols, drop = FALSE]
  
  if (ncol(mu_post) != K) {
    stop("Could not identify exactly K mu columns in posterior_samples.")
  }
  
  colnames(mu_post) <- paste0("p_", seq_len(K))
  
  if (!is.null(calc_differences)) {
    calc_differences <- convertVector2Matrix(calc_differences)
    mu_post <- calcDiffsMCMCNormal(
      posterior_samples = mu_post,
      calc_differences  = calc_differences
    )
  }
  
  if (!is.null(save_path) && !is.null(save_trial)) {
    saveRDS(
      posterior_samples,
      file = file.path(
        save_path,
        paste0("posterior_samples_", save_trial, "_", method_name, "_rds")
      )
    )
  }
  
  posterior2quantilesNormal(
    quantiles  = quantiles,
    posteriors = mu_post
  )
}

getPostQuantilesPooledNormal <- function(
    j_data,
    quantiles,
    calc_differences
) {
  stop("getPostQuantilesPooledNormal is not implemented for normal endpoints. Use bhm_normal instead.")
}

getPostQuantilesStratifiedNormal <- function(
    j_data,
    quantiles,
    calc_differences,
    n_mcmc_iterations
) {
  stop("getPostQuantilesStratifiedNormal is not implemented for normal endpoints. Use bhm_normal instead.")
}

getUniqueRowsNormal <- function(matrix) {
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  
  unique_rows <- stats::aggregate(
    id ~ .,
    data = cbind(id = seq_len(n_rows), matrix),
    FUN  = length
  )
  
  unique_rows[, seq_len(n_cols)]
}

getUniqueTrialsNormal <- function(scenario_list) {
  
  all_scenarios_n_subjects <- do.call(
    rbind,
    lapply(scenario_list, function(x) x$n_subjects)
  )
  
  all_scenarios_overall_gos <- do.call(
    rbind,
    lapply(scenario_list, function(x) x$previous_analyses$go_decisions)
  )[, 1]
  
  getUniqueRowsNormal(
    cbind(
      all_scenarios_n_subjects,
      go_flag = all_scenarios_overall_gos
    )
  )
}

loadAnalysesNormal <- function(
    scenario_numbers,
    analysis_numbers = rep(1, length(scenario_numbers)),
    load_path        = tempdir()
) {
  loadAnalyses(
    scenario_numbers = scenario_numbers,
    analysis_numbers = analysis_numbers,
    load_path        = load_path
  )
}

mapUniqueTrialsNormal <- function(
    scenario_list,
    method_quantiles_list,
    trials_unique_calc,
    applicable_previous_trials
) {
  method_names     <- names(method_quantiles_list)  # typically "normal"
  scenario_numbers <- sapply(scenario_list, function(x) x$scenario_number)
  
  hash_keys        <- getHashKeys(trials_unique_calc)
  hash_tables_list <- vector("list", length(method_quantiles_list))
  
  for (n in seq_along(hash_tables_list)) {
    hash_tables_list[[n]] <- createHashTable(hash_keys, method_quantiles_list[[n]])
  }
  
  exported_stuff <- c("convertVector2Matrix")
  "%do%" <- foreach::"%do%"
  
  scenario_method_quantiles_list <- foreach::foreach(
    k = seq_along(scenario_numbers),
    .verbose = FALSE,
    .export  = exported_stuff
  ) %do% {
    
    scenario_data_matrix <- scenario_list[[k]]$n_subjects
    
    if (applicable_previous_trials &&
        !is.null(scenario_list[[k]]$previous_analyses$post_quantiles)) {
      
      scenario_go_flags         <- scenario_list[[k]]$previous_analyses$go_decisions[, 1] > 0
      scenario_method_quantiles <- scenario_list[[k]]$previous_analyses$post_quantiles
      
    } else {
      
      scenario_go_flags         <- rep(TRUE, nrow(scenario_data_matrix))
      scenario_method_quantiles <- vector("list", length(method_names))
      names(scenario_method_quantiles) <- method_names
      
    }
    
    if (any(scenario_go_flags)) {
      
      scenario_data_matrix_go <- convertVector2Matrix(
        scenario_data_matrix[scenario_go_flags, , drop = FALSE]
      )
      search_keys <- getHashKeys(scenario_data_matrix_go)
      
      for (n in seq_along(method_names)) {
        scenario_method_quantiles[[method_names[n]]][scenario_go_flags] <-
          getHashValues(search_keys, hash_tables_list[[n]])
      }
    }
    
    scenario_method_quantiles
  }
  
  names(scenario_method_quantiles_list) <- paste0("scenario_", scenario_numbers)
  scenario_method_quantiles_list
}

performAnalysesNormal <- function(
    scenario_list,
    evidence_levels        = c(0.025, 0.05, 0.5, 0.8, 0.9, 0.95, 0.975),
    calc_differences       = NULL,
    n_mcmc_iterations      = 1e4,
    prior_parameters_normal= NULL,
    verbose                = TRUE
) {
  error_scenario_list <- simpleError(
    "Please provide an object of class 'scenario_list_normal' for 'scenario_list'")
  error_evidence_levels <- simpleError(
    "Please provide a vector of numerics in (0, 1) for the argument 'evidence_levels'")
  error_n_mcmc_iterations <- simpleError(
    "Please provide a positive integer for the argument 'n_mcmc_iterations'")
  error_verbose <- simpleError(
    "Please provide a logical for the argument 'verbose'")
  
  if (missing(scenario_list)) stop(error_scenario_list)
  if (!is.scenario_list_normal(scenario_list)) stop(error_scenario_list)
  
  if (!is.numeric.in.zero.one(evidence_levels)) stop(error_evidence_levels)
  if (!is.single.positive.wholenumber(n_mcmc_iterations)) stop(error_n_mcmc_iterations)
  if (!is.logical(verbose) || length(verbose) != 1L) stop(error_verbose)
  
  if (!is.null(calc_differences)) {
    calc_differences <- convertVector2Matrix(calc_differences)
  }
  
  quantiles <- sort(unique(round(
    1 - c(0.025, 0.05, 0.5, 0.8, 0.9, 0.95, 0.975, evidence_levels),
    9
  )))
  
  if (is.null(prior_parameters_normal)) {
    prior_parameters_normal <- getPriorParametersNormal()
  }
  
  prep         <- prepareAnalysisNormal(prior_parameters = prior_parameters_normal)
  j_parameters <- prep$j_parameters
  j_model_file <- prep$j_model_file
  j_data_fixed <- prep$j_data
  
  scenario_numbers <- sapply(scenario_list, function(x) x$scenario_number)
  
  all_scenarios_n_subjects <- do.call(
    rbind,
    lapply(scenario_list, function(x) x$n_subjects)
  )
  
  all_scenarios_overall_gos <- do.call(
    rbind,
    lapply(scenario_list, function(x) {
      if (!is.null(x$previous_analyses$post_quantiles)) {
        x$previous_analyses$go_decisions
      } else {
        matrix(TRUE, nrow = nrow(x$n_subjects), ncol = 1)
      }
    })
  )[, 1]
  
  trials_unique <- getUniqueTrialsNormal(scenario_list)
  
  n_cohorts <- ncol(trials_unique) - 1L
  
  applicable_previous_trials <- applicablePreviousTrialsNormal(
    scenario_list    = scenario_list,
    quantiles        = quantiles,
    n_cohorts        = n_cohorts,
    calc_differences = calc_differences
  )
  
  if (applicable_previous_trials) {
    calc_trial_indices <- trials_unique[, ncol(trials_unique)] > 0
  } else {
    calc_trial_indices <- rep(TRUE, nrow(trials_unique))
  }
  
  trials_unique_calc <- trials_unique[calc_trial_indices, , drop = FALSE]
  n_subjects_unique  <- trials_unique_calc[, seq_len(n_cohorts), drop = FALSE]
  
  if (verbose) {
    message(
      format(Sys.time(), "%d-%b-%Y"),
      " Performing Analyses (normal endpoint)"
    )
    message(
      "   Analyzing ",
      length(scenario_numbers), " scenario",
      ifelse(length(scenario_numbers) == 1, "", "s"),
      " (", nrow(n_subjects_unique), " unique trial realization",
      ifelse(nrow(n_subjects_unique) == 1, "", "s"), ")"
    )
  }
  
  if (verbose) {
    start_time <- Sys.time()
    message("   Running bhm_normal model ...")
  }
  
  method_quantiles_list <- getPostQuantilesNormal(
    quantiles         = quantiles,
    scenario_list     = scenario_list,
    n_subjects_unique = n_subjects_unique,
    calc_differences  = calc_differences,
    j_parameters      = j_parameters,
    j_model_file      = j_model_file,
    j_data_fixed      = j_data_fixed,
    n_mcmc_iterations = n_mcmc_iterations
  )
  
  if (verbose) {
    message(
      "       finished after ",
      round(Sys.time() - start_time, 1), " ",
      units(Sys.time() - start_time), "."
    )
  }
  
  ## map back to scenarios
  if (verbose) {
    start_time <- Sys.time()
    message("   Processing scenarios ...")
  }
  
  scenario_method_quantiles_list <- mapUniqueTrialsNormal(
    scenario_list              = scenario_list,
    method_quantiles_list      = method_quantiles_list,
    trials_unique_calc         = trials_unique_calc,
    applicable_previous_trials = applicable_previous_trials
  )
  
  if (verbose) {
    message(
      "       finished after ",
      round(Sys.time() - start_time, 1), " ",
      units(Sys.time() - start_time), "."
    )
  }
  
  ## build output
  analyses_list        <- vector("list", length(scenario_numbers))
  names(analyses_list) <- paste0("scenario_", scenario_numbers)
  
  for (s in seq_along(scenario_numbers)) {
    analyses_list[[s]] <- list(
      quantiles_list      = list(normal = scenario_method_quantiles_list[[s]]),
      scenario_data       = scenario_list[[s]],
      analysis_parameters = list(
        quantiles               = quantiles,
        evidence_levels         = evidence_levels,
        method_names            = "normal",
        prior_parameters_normal = prior_parameters_normal,
        n_mcmc_iterations       = n_mcmc_iterations
      )
    )
  }
  
  class(analyses_list) <- "analysis_list_normal"
  analyses_list
}

posterior2quantilesNormal <- function(quantiles, posteriors) {
  posteriors2Quantiles(
    quantiles  = quantiles,
    posteriors = posteriors
  )
}

prepareAnalysisNormal <- function(prior_parameters = NULL) {
  
  if (is.null(prior_parameters)) {
    prior_parameters <- getPriorParametersNormal()
  } else {
    if (!inherits(prior_parameters, "prior_parameters_normal")) {
      stop("prior_parameters must have class 'prior_parameters_normal'")
    }
  }
  
  j_data <- list(
    mu_pop_mean = prior_parameters$mu_pop_mean,
    prec_mu_pop = 1 / prior_parameters$mu_pop_sd^2,
    tau_shape   = prior_parameters$tau_shape,
    tau_rate    = prior_parameters$tau_rate,
    sigma_shape = prior_parameters$sigma_shape,
    sigma_rate  = prior_parameters$sigma_rate
  )
  
  j_model_file <- system.file(
    package   = "bhmbasket",
    "jags_models",
    "bhm_normal.txt",
    mustWork  = TRUE
  )
  
  j_parameters <- c("mu", "mu_pop", "prec_tau", "prec_sigma")
  
  list(
    j_parameters = j_parameters,
    j_model_file = j_model_file,
    j_data       = j_data
  )
}

print.analysis_listnormal <- function(x, digits = 2, ...) {
  print.analysis_list(x, digits = digits, ...)
}

qbetaDiffNormal <- function(
    quantiles,
    x_1_shape1,
    x_1_shape2,
    x_2_shape1,
    x_2_shape2,
    n_mcmc = 1e6
) {
  qbetaDiff(
    quantiles  = quantiles,
    x_1_shape1 = x_1_shape1,
    x_1_shape2 = x_1_shape2,
    x_2_shape1 = x_2_shape1,
    x_2_shape2 = x_2_shape2,
    n_mcmc     = n_mcmc
  )
}

saveAnalysesNormal <- function(
    analyses_list,
    save_path        = tempdir(),
    analysis_numbers = NULL
) {
  saveAnalyses(
    analyses_list    = analyses_list,
    save_path        = save_path,
    analysis_numbers = analysis_numbers
  )
}

