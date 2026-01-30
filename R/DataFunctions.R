is.scenario_list_normal <- function (x) {
  if (missing(x)) stop("Please provide an object for the argument 'x'")
  inherits(x, "scenario_list_normal")
}

getScenarioNormal <- function(
    n_subjects,
    true_means,
    cohort_names = seq_along(n_subjects),
    sd,
    n_trials = 1e4
) {
  if (missing(n_subjects))
    stop("Please provide 'n_subjects' (vector of positive integers).")
  if (missing(true_means))
    stop("Please provide 'true_means' (numeric vector).")
  if (missing(sd))
    stop("Please provide a single positive numeric for 'sd'.")
  
  if (length(n_subjects) != length(true_means))
    stop("n_subjects and true_means must have same length.")
  
  if (any(!is.positive.wholenumber(n_subjects)))
    stop("All entries in 'n_subjects' must be positive integers.")
  if (!is.single.numeric(sd) || sd <= 0)
    stop("sd must be a single positive numeric.")
  if (!is.single.positive.wholenumber(n_trials))
    stop("n_trials must be a single positive integer.")
  
  K <- length(n_subjects)
  trials <- vector("list", n_trials)
  
  for (t in seq_len(n_trials)) {
    
    y_list <- vector("list", K)
    
    for (k in seq_len(K)) {
      if (n_subjects[k] > 0) {
        y_list[[k]] <- stats::rnorm(n_subjects[k], mean = true_means[k], sd = sd)
      } else {
        y_list[[k]] <- numeric(0L)
      }
    }
    
    trials[[t]] <- list(
      y_list = y_list,
      y = y_list,
      n = n_subjects
    )
  }
  
  n_subjects_mat <- matrix(
    n_subjects,
    nrow = n_trials,
    ncol = K,
    byrow = TRUE
  )
  colnames(n_subjects_mat) <- paste0("n_", cohort_names)
  
  previous_gos <- matrix(
    TRUE,
    nrow = n_trials,
    ncol = K + 1L,
    byrow = TRUE
  )
  colnames(previous_gos) <- c("overall", paste0("decision_", cohort_names))
  
  scenario_data <- list(
    n_subjects        = n_subjects_mat,
    true_means        = true_means,
    sd                = sd,
    trials            = trials,
    previous_analyses = list(
      go_decisions   = previous_gos,
      post_quantiles = NULL
    ),
    n_trials          = n_trials,
    endpoint          = "normal_one_treat"
  )
  
  return(scenario_data)
}

#' @title simulateScenariosNormal
#' @description Create scenarios with normal endpoints for analysis with
#'   \code{performAnalysesNormal()}.
#' @param n_subjects_list A list that contains for each scenario a vector for
#'   the number of subjects per cohort. A single vector can be provided if all
#'   scenarios should have the same number of subjects.
#' @param mean_list A list that contains for each scenario a numeric vector
#'   with the true means per cohort.
#' @param sd A positive numeric (common SD for all cohorts and scenarios) or
#'   a list of numeric vectors (one per scenario, same length as mean vector).
#' @param scenario_numbers A vector of positive integers naming the scenarios,
#'   Default: \code{seq_along(mean_list)}.
#' @param n_trials A positive integer for the number of simulated trial
#'   realizations per scenario, Default: \code{10000}.
#'   If \code{n_trials} is present in \code{.GlobalEnv} and
#'   \code{missing(n_trials)}, the globally available value will be used.
#' @return An object of class \code{scenario_list_normal}.
#'   Each scenario contains:
#'   \itemize{
#'     \item \code{scenario_number}
#'     \item \code{n_subjects}: matrix (n_trials x K)
#'     \item \code{trials}: list of length n_trials; each element is a list
#'           with \code{y_list}, a list of K numeric vectors (one per cohort)
#'     \item \code{previous_analyses}: list (initially empty)
#'   }
#' @export
simulateScenariosNormal <- function(
    n_subjects_list,
    mean_list,
    sd,
    scenario_numbers = seq_along(mean_list),
    n_trials         = 1e4
) {
  
  error_n_subjects_list  <-
    "Provide a list of vectors of positive integers for 'n_subjects_list'."
  error_mean_list        <-
    "Provide a list of numeric vectors for 'mean_list'."
  error_sd               <-
    "Provide a positive numeric or a list of numeric vectors for 'sd'."
  error_n_trials         <-
    "Provide a positive integer for the argument 'n_trials'."
  error_scenario_numbers <-
    "Provide a vector of positive integers for the argument 'scenario_numbers'."
  
  checkmate::assert_list(
    mean_list,
    types       = "numeric",
    any.missing = FALSE,
    .var.name   = error_mean_list
  )
  
  ## allow a single vector for n_subjects_list
  if (!is.list(n_subjects_list)) {
    n_subjects_list <- rep(list(n_subjects_list), length(mean_list))
  }
  
  checkmate::assert_list(
    n_subjects_list,
    types       = "numeric",
    any.missing = FALSE,
    .var.name   = error_n_subjects_list
  )
  
  checkmate::assert_true(
    all(vapply(
      n_subjects_list,
      checkmate::test_integerish,
      logical(1),
      lower       = 1,
      any.missing = FALSE
    )),
    .var.name = error_n_subjects_list
  )
  
  checkmate::assert_integerish(
    scenario_numbers,
    lower       = 1,
    any.missing = FALSE,
    .var.name   = error_scenario_numbers
  )
  
  checkmate::assert_true(
    length(scenario_numbers) == length(n_subjects_list),
    .var.name = "'scenario_numbers' and 'n_subjects_list' must have same length"
  )
  
  checkmate::assert_true(
    length(n_subjects_list) == length(mean_list),
    .var.name = "'n_subjects_list' and 'mean_list' must have same length"
  )
  
  n_cohorts <- length(mean_list[[1L]])
  
  checkmate::assert_true(
    n_cohorts >= 2L,
    .var.name = "Each scenario must have at least 2 cohorts"
  )
  
  cohort_lengths_mu <- vapply(mean_list, length, integer(1))
  checkmate::assert_true(
    all(cohort_lengths_mu == n_cohorts),
    .var.name = "All scenarios must have same number of cohorts in 'mean_list'"
  )
  
  cohort_lengths_ns <- vapply(n_subjects_list, length, integer(1))
  checkmate::assert_true(
    all(cohort_lengths_ns == n_cohorts),
    .var.name = "All scenarios must have same number of cohorts in 'n_subjects_list'"
  )
  
  if (is.list(sd)) {
    checkmate::assert_true(
      length(sd) == length(mean_list),
      .var.name = "Length of 'sd' list must equal length of 'mean_list'"
    )
    for (i in seq_along(sd)) {
      checkmate::assert_numeric(
        sd[[i]],
        any.missing = FALSE,
        lower       = 0,
        .var.name   = error_sd
      )
      checkmate::assert_true(
        length(sd[[i]]) %in% c(1L, n_cohorts),
        .var.name = "Each 'sd'[[i]] must have length 1 or K"
      )
    }
  } else {
    checkmate::assert_number(sd, lower = 0, finite = TRUE, .var.name = error_sd)
    ## turn into scenario-wise list for convenience
    sd <- rep(list(rep(sd, length.out = n_cohorts)), length(mean_list))
  }
  
  if ("n_trials" %in% ls(envir = .GlobalEnv) && missing(n_trials)) {
    n_trials <- get("n_trials", envir = .GlobalEnv)
  }
  
  checkmate::assert_count(
    n_trials,
    positive  = TRUE,
    .var.name = error_n_trials
  )
  
  scenario_list <- vector("list", length = length(scenario_numbers))
  
  for (s in seq_along(scenario_numbers)) {
    
    n_subj_vec <- as.integer(n_subjects_list[[s]])
    mu_vec     <- mean_list[[s]]
    sd_vec     <- if (length(sd[[s]]) == 1L) rep(sd[[s]], n_cohorts) else sd[[s]]
    
    ## n_subjects matrix: n_trials x K (same n per trial)
    n_subjects_mat <- matrix(
      rep(n_subj_vec, each = n_trials),
      nrow = n_trials,
      ncol = n_cohorts,
      byrow = FALSE
    )
    
    ## trials: list of length n_trials; each contains y_list (one vector per cohort)
    trials <- vector("list", length = n_trials)
    
    for (t in seq_len(n_trials)) {
      y_list <- vector("list", length = n_cohorts)
      for (k in seq_len(n_cohorts)) {
        n_k        <- n_subj_vec[k]
        y_list[[k]] <- stats::rnorm(n_k, mean = mu_vec[k], sd = sd_vec[k])
      }
      trials[[t]] <- list(y_list = y_list)
    }
    
    scenario_list[[s]] <- list(
      scenario_number   = scenario_numbers[s],
      n_subjects        = n_subjects_mat,
      trials            = trials,
      previous_analyses = list(
        go_decisions   = matrix(1, nrow = n_trials, ncol = 1),   # all "go" initially
        post_quantiles = NULL
      )
    )
  }
  
  names(scenario_list) <- paste0("scenario_", scenario_numbers)
  class(scenario_list) <- "scenario_list_normal"
  
  return(scenario_list)
}

createTrialNormal <- function(
    n_subjects,
    true_means,
    sd
) {
  error_n_subjects <- simpleError(
    "Please provide a vector of non-negative integers for the argument 'n_subjects'")
  error_means <- simpleError(
    "Please provide a numeric vector for 'true_means'")
  error_sd <- simpleError(
    "Please provide a single positive numeric for the argument 'sd'")
  
  if (missing(n_subjects))  stop(error_n_subjects)
  if (missing(true_means))  stop(error_means)
  if (missing(sd))          stop(error_sd)
  
  if (any(!is.wholenumber(n_subjects)) || any(n_subjects < 0))
    stop(error_n_subjects)
  
  if (!is.numeric(true_means))
    stop(error_means)
  
  if (length(n_subjects) != length(true_means))
    stop("n_subjects and true_means must have same length")
  
  if (!is.single.numeric(sd) || sd <= 0) stop(error_sd)
  
  utils::capture.output({
    trial <- simulateScenariosNormal(
      n_subjects_list = list(n_subjects),
      mean_list       = list(true_means),
      sd              = sd,
      n_trials        = 1
    )
  })
  
  return(trial)
}

continueRecruitmentNormal <- function(
    n_subjects_add_list,
    decisions_list,
    method_name = NULL
) {
  error_n_subjects_add_list <- simpleError(
    "Please provide a list of vectors of non-negative integers for 'n_subjects_add_list'")
  error_decisions_list <- simpleError(
    "Please provide an object of class 'decision_list' for 'decisions_list'")
  error_method_name <- simpleError(
    "Please provide a valid 'method_name' present in decisions_list$scenario_1$decisions_list")
  
  if (missing(n_subjects_add_list)) stop(error_n_subjects_add_list)
  if (missing(decisions_list))      stop(error_decisions_list)
  
  if (!is.decision_list(decisions_list)) stop(error_decisions_list)
  
  if (is.null(method_name)) {
    n_methods <- length(decisions_list$scenario_1$decisions_list)
    if (n_methods > 1) {
      stop(error_method_name)
    } else {
      method_name <- names(decisions_list$scenario_1$decisions_list)
    }
  } else {
    if (!method_name %in% names(decisions_list$scenario_1$decisions_list))
      stop(error_method_name)
  }
  
  if (!is.list(n_subjects_add_list)) {
    n_subjects_add_list <- rep(list(n_subjects_add_list), length(decisions_list))
  }
  
  if (!is.list(n_subjects_add_list) ||
      any(!sapply(n_subjects_add_list, is.non.negative.wholenumber)))
    stop(error_n_subjects_add_list)
  
  scenario_numbers <- as.numeric(sub("scenario_", "", names(decisions_list)))
  
  if (length(n_subjects_add_list) != length(decisions_list)) {
    stop(simpleError(
      "The lengths of 'n_subjects_add_list' and 'decisions_list' must be equal"))
  }
  
  scenario_list <- vector("list", length(decisions_list))
  names(scenario_list) <- paste0("scenario_", scenario_numbers)
  
  for (s in seq_along(scenario_list)) {
    
    scen_data <- decisions_list[[s]]$scenario_data
    if (is.null(scen_data$endpoint) || scen_data$endpoint != "normal_one_treat") {
      stop("continueRecruitmentNormal: scenario_data$endpoint must be 'normal_one_treat'")
    }
    
    if (!(method_name %in%
          decisions_list[[s]]$analysis_data$analysis_parameters$method_names)) {
      stop(simpleError("Selected method_name not analyzed in this scenario"))
    }
    
    n_subjects_add <- n_subjects_add_list[[s]]
    
    n_subjects <- scen_data$n_subjects
    trials     <- scen_data$trials
    n_trials   <- scen_data$n_trials
    sd         <- scen_data$sd
    true_means <- scen_data$true_means
    
    K <- ncol(n_subjects)
    
    if (length(n_subjects_add) != K) {
      stop("For normal_one_treat endpoint, length of n_subjects_add must equal number of cohorts.")
    }
    if (nrow(n_subjects) != n_trials || length(trials) != n_trials) {
      stop("Inconsistent n_trials, n_subjects, or trials in scenario_data.")
    }
    
    go_decisions_all <- decisions_list[[s]]$decisions_list[[method_name]]
    previous_gos     <- go_decisions_all
    
    if ("overall" %in% colnames(go_decisions_all)) {
      overall_gos  <- go_decisions_all[, "overall"]
      go_decisions <- go_decisions_all[, colnames(go_decisions_all) != "overall", drop = FALSE]
    } else {
      overall_gos  <- rep(TRUE, nrow(go_decisions_all))
      go_decisions <- go_decisions_all
    }
    
    if (!identical(dim(go_decisions), dim(n_subjects))) {
      stop("go_decisions and n_subjects must have same dimension [n_trials x K] for normal_one_treat endpoint.")
    }
    
    for (t in seq_len(n_trials)) {
      if (!overall_gos[t]) next
      
      tr_t <- trials[[t]]
      
      for (k in seq_len(K)) {
        if (!go_decisions[t, k]) next
        
        add_n <- n_subjects_add[k]
        if (add_n <= 0) next
        
        y_new <- stats::rnorm(add_n, mean = true_means[k], sd = sd)
        
        tr_t$y[[k]] <- c(tr_t$y[[k]], y_new)
        tr_t$n[k]   <- tr_t$n[k] + add_n
        
        n_subjects[t, k] <- n_subjects[t, k] + add_n
      }
      
      trials[[t]] <- tr_t
    }
    
    scenario_list[[s]] <- list(
      n_subjects        = n_subjects,
      true_means        = true_means,
      sd                = sd,
      trials            = trials,
      previous_analyses = list(
        go_decisions   = previous_gos,
        post_quantiles = decisions_list[[s]]$analysis_data$quantiles_list
      ),
      n_trials          = n_trials,
      endpoint          = "normal_one_treat"
    )
    
    scenario_list[[s]]$scenario_number <- scen_data$scenario_number
  }
  
  class(scenario_list) <- c("scenario_list_normal", "scenario_list")
  return(scenario_list)
}

#' @title saveScenariosNormal
#' @md
#' @description Saves the normal-endpoint scenario data in a newly created or
#'   existing directory.
#' @param scenario_list An object of class `scenario_list_normal`,
#'   e.g. created with `simulateScenariosNormal()`.
#' @param save_path A string providing the path for the directory in which the
#'   scenario files should be created, Default: [base::tempdir()].
#' @return A named list of length 2 with the scenario numbers and the `save_path`.
#' @seealso
#'   [simulateScenariosNormal()],
#'   [loadScenariosNormal()],
#'   [base::tempdir()]
#' @export
saveScenariosNormal <- function(
    scenario_list,
    save_path = tempdir()
) {
  checkmate::assert(
    checkmate::check_class(scenario_list, "scenario_list_normal"),
    checkmate::check_character(save_path, len = 1),
    combine   = "and",
    .var.name = "Please provide an object of class 'scenario_list_normal' for the argument 'scenario_list'"
  )
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  scenario_numbers <- vapply(
    scenario_list,
    function(x) x$scenario_number,
    FUN.VALUE = integer(1L)
  )
  
  for (s in seq_along(scenario_list)) {
    saveRDS(
      scenario_list[[s]],
      file = file.path(save_path, paste0("scenario_data_", scenario_numbers[s], ".rds"))
    )
  }
  
  list(
    scenario_numbers = scenario_numbers,
    path             = save_path
  )
}

#' @title loadScenariosNormal
#' @md
#' @description
#' Load normal-endpoint scenarios saved with `saveScenariosNormal()`.
#'
#' @param scenario_numbers A vector of positive integers naming the scenarios to be loaded.
#' @param load_path A string for the directory where the scenarios are stored,
#'   Default: [base::tempdir()].
#'
#' @return
#' An object of class `scenario_list_normal`.
#'
#' @seealso
#'  \code{\link[bhmbasket]{simulateScenariosNormal}}
#'  \code{\link[bhmbasket]{saveScenariosNormal}}
#'  \code{\link[base]{tempdir}}
#'
#' @examples
#' \dontrun{
#'   scenarios_list <- simulateScenariosNormal(
#'     n_subjects_list = list(c(50, 50)),
#'     mean_list       = list(c(0, 1)),
#'     sd              = 1,
#'     n_trials        = 10
#'   )
#'
#'   save_info      <- saveScenariosNormal(scenarios_list)
#'   scenarios_list <- loadScenariosNormal(
#'     scenario_numbers = save_info$scenario_numbers,
#'     load_path        = save_info$path
#'   )
#' }
#' @export
loadScenariosNormal <- function(
    scenario_numbers,
    load_path = tempdir()
) {
  checkmate::assert_integerish(
    scenario_numbers,
    lower      = 1,
    any.missing = FALSE,
    .var.name  = "Providing a vector of positive integers for the argument 'scenario_numbers'"
  )
  
  checkmate::assert_string(
    load_path,
    .var.name = "Providing a string for the argument 'load_path'"
  )
  
  files <- file.path(load_path, paste0("scenario_data_", scenario_numbers, ".rds"))
  scenario_list <- lapply(files, readRDS)
  
  names(scenario_list) <- paste0("scenario_", scenario_numbers)
  class(scenario_list) <- "scenario_list_normal"
  
  return(scenario_list)
}

#' @export
print.scenario_list_normal <- function(x, ...) {
  
  n_scenarios    <- length(x)
  scenario_names <- names(x)
  
  # true means per cohort
  true_means   <- lapply(x, function(s) s$means)
  n_cohorts    <- length(true_means[[1L]])
  cohort_names <- paste0("c_", seq_len(n_cohorts))
  
  # average sample size per cohort over trials
  n_subjects_avg <- lapply(x, function(s) {
    colMeans(as.matrix(s$n_subjects))
  })
  
  # trial counts
  n_trial_realizations  <- x[[1L]]$n_trials
  n_unique_realizations <- nrow(getUniqueTrialsNormal(x))
  
  cat("scenario_list_normal of ", n_scenarios, " scenario",
      ifelse(n_scenarios == 1, "", "s"),
      " with ", n_cohorts, " cohort", ifelse(n_cohorts == 1, "", "s"),
      "\n\n", sep = "")
  
  for (n in seq_along(scenario_names)) {
    
    df_out <- rbind(
      "    - true means:"               = true_means[[n]],
      "    - average number of subjects:" = n_subjects_avg[[n]]
    )
    
    colnames(df_out) <- cohort_names
    
    cat("  -", scenario_names[n], "\n")
    print(df_out)
    cat("\n")
  }
  
  cat("  -", n_trial_realizations,  " trial realizations per scenario\n")
  cat("  -", n_unique_realizations, " unique trial realizations overall\n")
}