#' Find optimal progression criteria for a fixed sample size
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will enumerate all possible three-outcome decision rules (i.e. all lower and
#' upper thresholds) and calculate three opiating characteristics (OCs) for each.
#' If designs which satisfy the three upper constraints on these OCs exist, that
#' which gives the lowest average OC is returned.
#'
#' @param n Sample size.
#' @param rho_0 Null hypothesis.
#' @param rho_1 Alternative hypothesis.
#' @param sigma Standard deviation of outcome.
#' @param alpha_nom Nominal upper constraint on alpha.
#' @param beta_nom Nominal upper constraint on beta.
#' @param gamma_nom Nominal upper constraint on gamma.
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold; or NULL when no valid designs exist.
#' @export
#'
#' @examples
#' n <- 100
#' rho_0 <- 0
#' rho_1 <- 0.3
#' sigma <- 1
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#' gamma_nom <- 0.9
#'
#' opt_pc_cont(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
#' 
opt_pc_cont <- function(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom){

  
  
  # Find which designs satisfy all operating characteristic constraints
  valid <- alpha <= alpha_nom & beta <= beta_nom & gamma <= gamma_nom
  
  if(sum(valid) > 0){
    # Compute the average of all operating characteristics for all designs
    mean_ocs <- rowMeans(cbind(alpha, beta, gamma_L + gamma_U))
    # Get index of the valid design with minimal mean OC
    j <- which.min(mean_ocs[valid])
    # Get corresponding index of all designs and note the result
    i <- which(valid)[j]
    design <- c(n, df[i, "x0"], df[i, "x1"])
  } else {
    design <- NULL
  }
  return(design)
}

get_x_0 <- function(x_1, n, rho_0, rho_1, sigma, alpha_nom){
  # For any given x_1, we can find the value of x_0 needed
  # to give exactly alpha = alpha_nom (and return NA if this 
  # is not possible)
  z <- 2 - 2*alpha_nom - pnorm(x_1)
  if(z <= 1){
    x_0 <- qnorm(2 - 2*alpha_nom - pnorm(x_1))
  } else {
    x_0 <- NA
  }
}

objective_f <- function(x_0, n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom)

x_1s <- seq(0, 10, 0.1)
x_0s <- NULL
for(i in x_1s){
  x_0s <- c(x_0s, get_x_0(i, n, rho_0, rho_1, sigma, alpha_nom))
}

# find x_1 which leads to beta = beta_nom, then check if viable w.r.t.
# gamma < gamma_nom. Note difference to binary case where avg OCs was 
# minimised when choosing between multiple valid options. Consider revising
# to make both consistent.


