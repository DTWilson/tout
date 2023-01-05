#' Find optimal sample size and progression criteria
#' 
#' Given a null and alternative hypothesis, this function searches for the 
#' lowest sample size such that a design with optimal progression criteria (as 
#' determined by the function `opt_pc`) satisfies upper constraints on three
#' operating characteristics.
#'
#' @param n Optional sample size.
#' @param rho_0 Null hypothesis.
#' @param rho_1 Alternative hypothesis.
#' @param alpha_nom Nominal upper constraint on alpha.
#' @param beta_nom Nominal upper constraint on beta.
#' @param gamma_nom Nominal upper constraint on gamma.
#' @param max_n Optional upper limit to use in search over sample sizes.
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
#' opt_design(rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
#' 
opt_design <-  function(rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, max_n = NULL, n = NULL)
{
  if(is.null(n)){
    min_n <- 2
    if(is.null(max_n)){
      # Get sample size for standard two outcome design taking a normal approx
      # and making conservative assumption on variance (maximised at rho = 0.5)
      n_two <- 0.25*(stats::qnorm(1 - alpha_nom) - stats::qnorm(beta_nom))^2/(rho_1 - rho_0)^2
      # Set max_n at an (arbitrarily) large multiple of this
      max_n <- floor(5*n_two)
    }
    # Run a bisection search to find the smallest n which returns a valid
    # design. Note that bisection assumes monotonicity which does not hold here
    # (only approximately), so consider supplementing by a restricted exact 
    # local search.
    while((min_n + 1) < max_n){
      n <- floor((min_n + max_n)/2)
      
      design <- opt_pc(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
      
      if(is.null(design)){
        min_n <- n
      } else {
        final_design <- design
        max_n <- n
      } 
    }
  } else {
    final_design <- opt_pc(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
  }
  return(final_design)
}