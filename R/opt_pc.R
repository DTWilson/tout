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
#' rho_0 <- 0.5
#' rho_1 <- 0.7
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#' gamma_nom <- 0.9
#'
#' opt_pc(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
#' 
opt_pc <- function(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom){
  # Create a dataframe of all possible progression criteria for given n
  df <- expand.grid(n = n,
                    x0 = 0:n,
                    x1 = 0:n)
  df <- df[df$x0 <= df$n & df$x1 <= df$n & df$x0 <= df$x1,]

  # Calculate the error rates for each design
  ocs <- get_ocs(df$n, df$x0, df$x1, rho_0, rho_1)

  # Find which designs satisfy all operating characteristic constraints
  valid <- ocs[,1] <= alpha_nom & ocs[,2] <= beta_nom & ocs[,3] <= gamma_nom

  if(sum(valid) > 0){
    # Compute the average of all operating characteristics for all designs
    mean_ocs <- rowMeans(ocs)
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
