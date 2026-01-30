getPriorParametersNormal <- function(
    mu_pop_mean = 0,
    mu_pop_sd   = 100,
    tau_shape   = 0.5,
    tau_rate    = 0.05,
    sigma_shape = 0.5,
    sigma_rate  = 0.05
) {
  if (!is.single.numeric(mu_pop_mean))
    stop("mu_pop_mean must be a single numeric.")
  if (!is.single.positive.numeric(mu_pop_sd))
    stop("mu_pop_sd must be a single positive numeric.")
  if (!is.single.positive.numeric(tau_shape))
    stop("tau_shape must be a single positive numeric.")
  if (!is.single.positive.numeric(tau_rate))
    stop("tau_rate must be a single positive numeric.")
  if (!is.single.positive.numeric(sigma_shape))
    stop("sigma_shape must be a single positive numeric.")
  if (!is.single.positive.numeric(sigma_rate))
    stop("sigma_rate must be a single positive numeric.")
  
  prior_parameters <- list(
    mu_pop_mean = mu_pop_mean,
    mu_pop_sd   = mu_pop_sd,
    tau_shape   = tau_shape,
    tau_rate    = tau_rate,
    sigma_shape = sigma_shape,
    sigma_rate  = sigma_rate
  )
  
  class(prior_parameters) <- "prior_parameters_normal"
  prior_parameters
}

setPriorParametersNormal <- function(
    mu_pop_mean,
    mu_pop_sd,
    tau_shape,
    tau_rate,
    sigma_shape,
    sigma_rate
) {
  if (missing(mu_pop_mean)) stop("Please provide 'mu_pop_mean'.")
  if (missing(mu_pop_sd))   stop("Please provide 'mu_pop_sd'.")
  if (missing(tau_shape))   stop("Please provide 'tau_shape'.")
  if (missing(tau_rate))    stop("Please provide 'tau_rate'.")
  if (missing(sigma_shape)) stop("Please provide 'sigma_shape'.")
  if (missing(sigma_rate))  stop("Please provide 'sigma_rate'.")
  
  getPriorParametersNormal(
    mu_pop_mean = mu_pop_mean,
    mu_pop_sd   = mu_pop_sd,
    tau_shape   = tau_shape,
    tau_rate    = tau_rate,
    sigma_shape = sigma_shape,
    sigma_rate  = sigma_rate
  )
}