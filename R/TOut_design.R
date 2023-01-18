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
#' @param gamma_nom nominal upper constraint on gamma.
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
#' gamma_nom <- 0.5
#'
#' TOut_design_bin(rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
#' 
#' tau <- c(0.08, 0.12)
#' 
#' TOut_design_bin(rho_0, rho_1, alpha_nom, beta_nom, tau = tau)
#' 
TOut_design_bin <-  function(rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, eta = 0.5, tau = NULL, max_n = NULL){
  
  check_tau(tau, eta)
  
  if(is.null(max_n)){
    # Get sample size for standard two outcome design taking a normal approx
    # and making conservative assumption on variance (maximised at rho = 0.5)
    n_two <- 0.25*(stats::qnorm(1 - alpha_nom) - stats::qnorm(beta_nom))^2/(rho_1 - rho_0)^2
    # Set max_n at an (arbitrarily) large multiple of this
    max_n <- floor(5*n_two)
  }
  results <- NULL
  for(n in 0:max_n){
    if(is.null(tau)){
      results <- rbind(results, opt_pc_bin(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, eta))
    } else {
      results <- rbind(results, opt_pc_adjust_bin(n, rho_0, rho_1, alpha_nom, beta_nom,
                                           tau_min = tau[1], tau_max = tau[2]))
    }
  }
  # TO Do: allow for cases when no feasible design is found
  return(results[!is.na(results[,1]),][1,])
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
#' @param gamma_nom nominal upper constraint on gamma.
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
#' gamma_nom <- 0.5
#'
#' TOut_design_cont(rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom)
#' 
#' tau <- c(0.08, 0.12)
#' 
#' TOut_design_cont(rho_0, rho_1, sigma, alpha_nom, beta_nom, tau = tau)
#' 
TOut_design_cont <-  function(rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom, eta = 0.5, tau = NULL, max_n = NULL){
  
  check_tau(tau, eta)
  
  if(is.null(max_n)){
    n_two <- 1*(stats::qnorm(1 - alpha_nom) - stats::qnorm(beta_nom))^2/(rho_1 - rho_0)^2
    # Set max_n at an (arbitrarily) large multiple of this
    max_n <- floor(5*n_two)
  }
  results <- NULL
  for(n in 0:max_n){
    if(is.null(tau)){
      results <- rbind(results, opt_pc_cont(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, 
                                       gamma_nom, eta))
    } else {
      results <- rbind(results, opt_pc_adjust_cont(n, rho_0, rho_1, sigma, alpha_nom, beta_nom,
                                            tau_min = tau[1], tau_max = tau[2]))
    }
  }
  # TO Do: allow for cases when no feasible design is found
  return(results[!is.na(results[,1]),][1,])
}

check_tau <- function(tau, eta){
  if(!is.null(tau)){
    if(length(tau) != 2){
      stop("tau must be a two-element vector giving lower and upper bounds
                              of the adjustment effect.")
    } else {
      if(tau[2] <= tau[1]) stop("Upper limit of tau must be less than or equal to
                                lower limit.")
    }
  }
}