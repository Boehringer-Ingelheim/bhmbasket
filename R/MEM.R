#' @importFrom itertools isplitVector

#using log space
# but the prior matrix shouldn't have 0 entries!!
log_mem_prior_mat <- function(m, prior_inclusion) {
  mem <- m[upper.tri(m)]
  source_vec <- prior_inclusion[upper.tri(prior_inclusion)]
  log_s_in <- log(source_vec)
  log_s_ex <- log(1 - source_vec)
  sum(mem * log_s_in + (1 - mem) * log_s_ex) #given out in log space
}

# using log space and vectorization
log_marg_dens_mcmc <- function(m, xvec, nvec, avec, bvec, beta_vec, prod_vec) {
  m_dot_xvec <- m %*% xvec
  m_dot_n_minus_xvec <- m %*% (nvec - xvec)
  term1 <- lbeta(avec + m_dot_xvec, bvec + m_dot_n_minus_xvec) - lbeta(avec, bvec)
  term2 <- (1 - m) %*% log(prod_vec)
  sum(term1 + term2)
}
#log_marg_dens_mcmc(matrix(c(1,0,0,0,1,0,0,0,1), ncol=3), c(1,2,3), c(4,4,4), c(.5, .5, .5), c(.5, .5, .5), beta(c(.5, .5, .5), c(.5, .5, .5)), beta(c(1,2,3)+c(.5, .5, .5), c(4,4,4)-c(1,2,3)+c(.5, .5, .5))/beta(c(.5, .5, .5), c(.5, .5, .5)))
#log_marg_dens_mcmc_opt(matrix(c(1,0,0,0,1,0,0,0,1), ncol=3), c(1,2,3), c(4,4,4), c(.5, .5, .5), c(.5, .5, .5), beta(c(.5, .5, .5), c(.5, .5, .5)), beta(c(1,2,3)+c(.5, .5, .5), c(4,4,4)-c(1,2,3)+c(.5, .5, .5))/beta(c(.5, .5, .5), c(.5, .5, .5)))

# Function to compute the change in log marginal density
compute_delta_log_marg_dens <- function(m_old, m_prop, xvec, nvec, avec, bvec, beta_vec, prod_vec) {
  changed_rows <- which(rowSums(m_old != m_prop) > 0)
  delta <- 0

  for (i in changed_rows) {
    mi_old <- m_old[i, ]
    mi_prop <- m_prop[i, ]
    term_old <- lbeta(avec[i] + sum(mi_old * xvec), bvec[i] + sum(mi_old * (nvec - xvec))) - lbeta(avec[i], bvec[i]) +
      sum((1 - mi_old) * log(prod_vec))
    term_prop <- lbeta(avec[i] + sum(mi_prop * xvec), bvec[i] + sum(mi_prop * (nvec - xvec))) - lbeta(avec[i], bvec[i]) +
      sum((1 - mi_prop) * log(prod_vec))
    delta <- delta + (term_prop - term_old)
  }
  return(delta)
}

# Function to compute the change in log prior
compute_delta_log_prior <- function(m_old, m_prop, prior_inclusion) {
  mem_old <- m_old[upper.tri(m_old)]
  mem_prop <- m_prop[upper.tri(m_prop)]
  source_vec <- prior_inclusion[upper.tri(prior_inclusion)]
  log_s_in <- log(source_vec)
  log_s_ex <- log(1 - source_vec)
  term_old <- sum(mem_old * log_s_in + (1 - mem_old) * log_s_ex)
  term_prop <- sum(mem_prop * log_s_in + (1 - mem_prop) * log_s_ex)
  delta <- term_prop - term_old
  return(delta)
}

#Optimized update_mh function
update_mh <- function(m_old, m, xvec, nvec, avec, bvec, prior_ep, beta_vec, old_dens, prod_vec) {
  if (is.na(old_dens)) {
    old_dens <- log_marg_dens_mcmc(m_old, xvec, nvec, avec, bvec, beta_vec, prod_vec) +
      log_mem_prior_mat(m_old, prior_ep)
  }

  #Sample indices to flip
  k <- max(m, na.rm = TRUE)
  # v <- sample(1:k, #sample uniformely out of the numbers 1 to k
  #             seq_len(k)[which(rmultinom( #a certain number of samples
  #               1, 1,
  #               (rev(seq_len(k))^3) / sum(seq_len(k)^3) #namely also 1-k an the prob for the amount of draws is p(#1)= 10^3/sum, ... p(#10)=1^3/sum
  #               #sampling without replacement
  #             ) == 1)]
  # )
  v <- sample(1:k, size = 1, prob = (rev(seq_len(k))^3) / sum(seq_len(k)^3))
  # Flip selected elements (instead of flip_mem)
  m_prop <- m_old
  indices_to_flip <- which(m == v)
  m_prop[indices_to_flip] <- 1 - m_prop[indices_to_flip]#if the entry in m_old is 1 change it to 0, otherwise to 1


  # Calculate change in density and not the new density
  delta_log_marg_dens <- compute_delta_log_marg_dens(m_old, m_prop, xvec, nvec, avec, bvec, beta_vec, prod_vec)
  delta_log_prior <- compute_delta_log_prior(m_old, m_prop, prior_ep)


  rho <- exp(delta_log_marg_dens + delta_log_prior)

  if (rho >= runif(1)) {
    old_dens <- old_dens + delta_log_marg_dens + delta_log_prior
    out <- m_prop
  } else {
    out <- m_old
  }

  list(out, old_dens)
}


i_models <- function(hh, models, samp) {
  k <- length(models[1, ])
  if (hh == 1) {
    ii <- seq_len(k)
  } else if (hh == k) {
    ii <- c(k, seq_len(k - 1))
  } else {
    ii <- c(hh, seq_len(hh - 1), (hh + 1):k)
  }
  which(apply(models,
              MARGIN = 1,
              FUN = function(x, t) {
                sum(x == t)
              },
              t = samp[hh, ii]
  ) == k)
}

models_count1 <- function(samp, models) {
  out <- matrix(0, nrow(models), ncol(models)) # NxN matrix with zero-entries
  u <- vapply(seq_len(ncol(models)),
              FUN = i_models,
              FUN.VALUE = NA_integer_,
              models = models,
              samp = samp
  )
  for (i in seq_along(u)) {
    out[u[i], i] <- 1
  }
  out
}

gen_post <- function(x, n, omega, a, b) {
  alpha <- a + omega %*% x
  beta <- b + (omega %*% n - omega %*% x)
  rbeta(1, alpha, beta)
}

samp_post <- function(x, n, omega, w, a, b) {
  gen_post(x, n, omega[which(rmultinom(1, 1, w) == 1), ], a, b)
}

sample_posterior_model <- function(model, num_samples = 100000) {
  k <- length(model$responses) #amount of baskets
  ret <- matrix(nrow = num_samples, ncol = k) #matrix, rows = amount of samples

  for (j in 1:k) {
    #for each basket
    if (k == 1) {
      ii <-c(1)
    } else if(j == k) {
      ii <- c(k, seq_len(k - 1))
    } else{
      ii <- c(j, seq_len(j - 1), (j + 1):k)
    } #how one should read out the


    ret[,j] <- replicate(
      num_samples,samp_post(
        model$responses[ii],
        model$size[ii],
        model$models,
        model$pweights[[j]],
        model$shape1[j],
        model$shape2[j]
      ))
  }

  dimnames(ret) <- list(NULL, model$name)
  ret
}


# prior_matrix<-function(responses, size){
#   rates=responses/size
#   a1<-matrix(rates, nrow=length(rates), ncol=length(rates))
#   a2<-matrix(rates, nrow=length(rates), ncol=length(rates), byrow=TRUE)
#   return(1- abs(a1-a2))
# }

prior_matrix <- function(responses, size){
  rates = responses / size
  a1 <- matrix(rates, nrow = length(rates), ncol = length(rates))
  a2 <- matrix(rates, nrow = length(rates), ncol = length(rates), byrow = TRUE)
  result <-  abs(a1 - a2)
  result2<-result
  threshold<-max(result)/2
  result2[result<threshold]<-0.99
  result2[result>=threshold]<-0.01
  #diag(result)<-0
  # Modify non-diagonal elements
  # result[lower.tri(result)] <- ifelse(result[lower.tri(result)] == 1, 0.99, result[lower.tri(result)])
  # result[upper.tri(result)] <- ifelse(result[upper.tri(result)] == 1, 0.99, result[upper.tri(result)])
  # 
  # Modify diagonal elements
  diag(result2) <- 1
  
  return(result2)
}

mem_mcmc_samples <- function(responses,
                             size,
                             name,
                             p0 = 0.15,
                             shape1 = 0.5,
                             shape2 = 0.5,
                             prior = 
                               diag(length(responses)) / 2 +
                               matrix(0.5,
                                      nrow = length(responses),
                                      ncol = length(responses)
                               ),
                             hpd_alpha = 0.05,
                             alternative = "greater",
                             mcmc_iter = 200000,
                             mcmc_burnin = 50000,
                             initial_mem = round(prior - 0.001),
                             seed = 1000,
                             cluster_analysis = FALSE,
                             call = NULL
) {
 # set.seed(seed)
  mcmc_iter <- mcmc_iter - mcmc_burnin
 prior <- prior_matrix(responses, size)
  print("hi") #prior)
  if (length(shape1) == 1) {
    shape1 <- rep(shape1, length(responses))
  }
  if (length(shape2) == 1) {
    shape2 <- rep(shape2, length(responses))
  }
  
  if (length(p0) == 1) {
    p0 <- rep(p0, length(responses))
  }
  
  size1 <- size[size != 0]
  alp <- hpd_alpha
  if (length(size1) < 1) {
    stop(red(
      "The length of the responses must be equal or greater than 1"
    ))
  }
  
  if (length(size1) == 1) {
    ind <- which(size != 0)
    n_vec <- length(size)
    
    pweights <- rep(0, n_vec)
    pweights[ind] <- 1
    
    t <-eval_post_one_group(p0[1], responses[1], size[1], shape1[1], shape2[1], alternative)
    
  }
  
  if (!isTRUE(all.equal(diag(prior), rep(1, ncol(prior))))) {
    stop(red("Elements on the main diagonal of `prior` must be 1."))
  }
  
  
  
  # Error if the inital_mem isn't symmetric.
  if (!isTRUE(all.equal(initial_mem, t(initial_mem)))) {
    stop(red("The `initial_mem` matrix must be symmetric."))
  }
  if (!isTRUE(all(diag(initial_mem) == 1))) {
    stop(red("The main diagonal of the `initial_mem` matrix must be 1's."))
  }
  
  m_old <- initial_mem
  
  
  ### Create Map for Proposal Distribution ###
  m <- diag(NA, nrow(m_old))
  k <- 1
  for (ii in seq_len(nrow(m_old) - 1)) {
    for (jj in (ii + 1):ncol(m_old)) {
      m[ii, jj] <- m[jj, ii] <- k
      k <- k + 1
    }
  }
  ### Implement Metropolis-Hastings Alg ###
  if (missing(name)) {
    name <- paste("basket", seq_along(size))
  }
  if (is.factor(name)) {
    name <- as.character(name)
  }
  
  
  ### Produce sample space of MEM ###
  
  mod_mat <- as.matrix(expand.grid(rep(list(0:1), length(responses)-1)))
  mod_mat[order(rowSums(mod_mat)), ]
  
  models <- cbind(1, mod_mat)
  mweights <- matrix(0, nrow(models), length(responses))
  colnames(mweights) <- name
  mem_samp <- list(m_old)
  mweights <- mweights + models_count1(samp = mem_samp[[1]], models = models) #why those we add the initial mem to the mems and not only the ones after the burnin
  
  old_dens <- NA
  
  beta_vec <- beta(shape1, shape2) #the prior of the response rates for the baskets
  prod.vec <- beta(responses + shape1, size + shape2 - responses) / beta(shape1, shape2)
  #The **marginal likelihood
  
  
  #burnin period m_old ist the MEM matrix, old_dens is the margnial distribution for the MEM matrix
  for (i in 1:mcmc_burnin) {
    t <- update_mh(
      m_old, m, xvec=responses, nvec=size,
      avec=shape1, bvec=shape2,  prior_ep=prior, beta_vec=beta_vec, old_dens=old_dens, prod_vec=prod.vec
    )
    
    m_old <- t[[1]]
    old_dens <- t[[2]]
  }
  
  
  
  
  #the iterations we save
  for (kk in seq_len(mcmc_iter)[-1]) {
    t <- update_mh(
      mem_samp[[kk - 1]], m, responses, size,
      shape1, shape2,  prior, beta_vec, old_dens, prod.vec
    )
    mem_samp[[kk]] <- t[[1]]
    old_dens <- t[[2]]
  }
  
  it <- NULL

  models_count <-
    foreach(
      it = itertools::isplitVector(seq_len(mcmc_iter)[-1], chunks = nbrOfWorkers()),
      .combine = c,
      .options.future = list(seed = TRUE)
    ) %dofuture% {
      foreach(k = it) %do% {
        models_count1(samp = mem_samp[[k]], models = models)
      }
    }
  
  for (kk in seq_len(mcmc_iter)[-1]) {
    mweights <- mweights + models_count[[kk - 1]]
  }
  
  # Compute posterior model weights
  pweights <- lapply(seq_len(ncol(mweights)), function(kk) mweights[, kk] / mcmc_iter)
  
  # List for post-processing
  return(list(samples=sample_posterior_model(list(
    responses = responses,
    size = size,
    name = name,
    shape1 = shape1,
    shape2 = shape2,
    models = models,
    pweights = pweights,
    p0 = p0,
    alpha = hpd_alpha,
    alternative = alternative
  )), Omega = mweights/mcmc_iter, Encoder = models, Prior = prior))
  
}