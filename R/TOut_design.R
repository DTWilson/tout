#' Find optimal sample size and progression criteria
#' 
#' Given a null and alternative hypothesis, this function finds the 
#' lowest sample size such that a design with optimal progression criteria (as 
#' determined by the function `opt_pc_bin`) satisfies upper constraints on three
#' operating characteristics.
#'
#' @param rho_0 null hypothesis.
#' @param rho_1 alternative hypothesis.
#' @param alpha_nom nominal upper constraint on alpha.
#' @param beta_nom nominal upper constraint on beta.
#' @param eta probability of an incorrect decision under the null or alternative
#' after an intermediate result. Defaults to 0.5.
#' @param tau two element vector denoting lower and upper limits of the 
#' effect of adjustment.
#' @param max_n optional upper limit to use in search over sample sizes.
#' 
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold; or NULL when no valid designs exist.
#' @export
#'
#' @examples
#' rho_0 <- 0.5
#' rho_1 <- 0.7
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#'
#' TOut_design_bin(rho_0, rho_1, alpha_nom, beta_nom)
#' 
#' tau <- c(0.08, 0.12)
#' 
#' TOut_design_bin(rho_0, rho_1, alpha_nom, beta_nom, tau = tau)
#' 
TOut_design_bin <-  function(rho_0, rho_1, alpha_nom, beta_nom, gamma_nom = 1, eta = 0.5, tau = c(0,0), max_n = NULL){
  
  check_tau(tau, eta)
  
  if(is.null(max_n)){
    # Get sample size for standard two outcome design taking a normal approx
    # and making conservative assumption on variance (maximised at rho = 0.5)
    n_two <- 0.25*(stats::qnorm(1 - alpha_nom) - stats::qnorm(beta_nom))^2/(rho_1 - rho_0)^2
    # Set max_n at an (arbitrarily) large multiple of this
    max_n <- floor(5*n_two)
  }

  # An exhaustive search here due to the non-monotonic relationship with n
  n <- 0
  valid <- FALSE
  while(!valid & n <= max_n){
    n <- n + 1
    design <- opt_pc_bin(n, rho_0, rho_1, alpha_nom, beta_nom, 
                          tau=tau, eta=eta)
    if(design$valid & (design$gamma <= gamma_nom)) {
      final_design <- design
      valid <- TRUE
    }
  }
  
  if(valid){
    return(final_design)
  } else {
    cat("No valid design found. Consider increasing the maximum sample size (max_n).")
  }
}

#' Find optimal sample size and progression criteria
#' 
#' Given a null and alternative hypothesis, this function finds the 
#' lowest sample size such that a design with optimal progression criteria (as 
#' determined by the function `opt_pc_cont`) satisfies upper constraints on three
#' operating characteristics.
#'
#' @param rho_0 null hypothesis.
#' @param rho_1 alternative hypothesis.
#' @param sigma standard deviation of outcome.
#' @param alpha_nom nominal upper constraint on alpha.
#' @param beta_nom nominal upper constraint on beta.
#' @param eta probability of an incorrect decision under the null or alternative
#' after an intermediate result. Defaults to 0.5.
#' @param tau two element vector denoting lower and upper limits of the 
#' effect of adjustment.
#' @param max_n optional upper limit to use in search over sample sizes.
#' 
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold; or NULL when no valid designs exist.
#' @export
#'
#' @examples
#' rho_0 <- 0
#' rho_1 <- 0.3
#' sigma <- 1
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#'
#' TOut_design_cont(rho_0, rho_1, sigma, alpha_nom, beta_nom)
#' 
#' tau <- c(0.08, 0.12)
#' 
#' TOut_design_cont(rho_0, rho_1, sigma, alpha_nom, beta_nom, tau = tau)
#' 
TOut_design_cont <-  function(rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom = 1, eta = 0.5, tau = c(0,0), max_n = NULL){
  
  check_tau(tau, eta)
  
  if(is.null(max_n)){
    n_two <- 1*(stats::qnorm(1 - alpha_nom) - stats::qnorm(beta_nom))^2/(rho_1 - rho_0)^2
    # Set max_n at an (arbitrarily) large multiple of this
    max_n <- floor(50*n_two)
  }
  
  min_n <- 0
  valid <- FALSE
  while((max_n - min_n) > 1){
    n <- ceiling((min_n + max_n)/2)
    design <- opt_pc_cont(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, 
                          tau=tau, eta=eta)
    if(design$valid & (design$gamma <= gamma_nom)) {
      max_n <- n
      final_design <- design
      valid <- TRUE
    } else {
      min_n <- n
    }
  }
  
  if(valid){
    return(final_design)
  } else {
    cat("No valid design found. Consider increasing the maximum sample size (max_n).")
  }
}

check_tau <- function(tau, eta){
  if(!is.null(tau)){
    if(length(tau) != 2){
      stop("tau must be a two-element vector giving lower and upper bounds
                              of the adjustment effect.")
    } else {
      if(tau[2] < tau[1]) stop("Upper limit of tau must be less than or equal to
                                lower limit.")
    }
  }
}