#' Find optimal progression criteria for a fixed sample size and binary outcome
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will determine upper and lower progression criteria such that alpha is controlled
#' exactly, beta is controlled or minimised when control is not feasible, and gamma
#' is minimised.
#'
#' @param n sample size.
#' @param rho_0 null hypothesis.
#' @param rho_1 alternative hypothesis.
#' @param alpha_nom nominal upper constraint on alpha.
#' @param beta_nom nominal upper constraint on beta.
#' @param tau vector with lower and upper bounds of adjustment effect.
#' @param eta probability of an incorrect decision after an intermediate result. 
#' Defaults to 0.5.
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold (or NA when no valid designs exist), and 
#' operating characteristics alpha, beta, gamma. Decision thresholds are on the 
#' outcome scale.
#' @export
#'
#' @examples
#' n <- 100
#' rho_0 <- 0.5
#' rho_1 <- 0.7
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#'
#' opt_pc_bin(n, rho_0, rho_1, alpha_nom, beta_nom)
#' 
opt_pc_bin <- function(n, rho_0, rho_1, alpha_nom, beta_nom, 
                       tau = c(0,0), eta = 0.5){
  
  tau_min <- tau[1]
  tau_max <- tau[2]
  
  # Check that the arguments are specified correctly
  check_arguments(n, alpha_nom, beta_nom, eta)
  check_arguments_bin(rho_0, rho_1)
  
  # Get minimum x_1 s.t. alpha can be controlled, and default max x_1
  min_x_1 <- min_x_1_bin(n, rho_0, alpha_nom, tau_min, eta)
  max_x_1 <- n

  # Find optimal choice of x_1 - that which gives beta ~ beta_nom
  # Run an exhaustive search over all possible choices of x_1
  x_1s <- min_x_1:max_x_1
  # For each, find the optimal x_0
  x_0s <- opt_x_0_bin(x_1s, n, rho_0, alpha_nom, tau_min, eta)
  # Get resulting betas
  betas <- get_ocs_bin(n, x_0s, x_1s, rho_0, rho_1, tau_min, tau_max, eta)[,2]
  # Convert betas to a measure to be minimised when optimising
  to_min <- beta_objective(betas, beta_nom, x_0s, x_1s)
  x_0 <- x_0s[which.min(to_min)]
  x_1 <- x_1s[which.min(to_min)]
  
  ocs <- get_ocs_bin(n, x_0, x_1, rho_0, rho_1, tau_min, tau_max, eta)
  
  # Check if all constraints are (approximately) satisfied
  valid <- FALSE
  if(all(ocs[1:2] < c(alpha_nom, beta_nom))){
    valid <- TRUE
  }
  
  structure(list(valid = valid, n = n, thresholds = c(x_0, x_1), 
                 alpha = ocs[1], beta = ocs[2], gamma = ocs[3],
                 hyps = c(rho_0, rho_1), tau = tau, eta = eta),
            class = "tout_design_bin")
}

#' @importFrom ggplot2 aes
#' @export
plot.tout_design_bin <- function(x, y, ...){
  # Vector of possible outcomes
  ys <- 0:x$n
  # Probabilities of these outcomes under the null and alternative hypotheses
  probs_null <- dbinom(ys, size = x$n, prob = x$hyps[1])
  probs_alt <- dbinom(ys, size = x$n, prob = x$hyps[2])
  
  # Data frame for plotting, flagging all direct and indirect errors
  df <- data.frame(y = ys,
                   p_n = probs_null,
                   tI = c(rep("None", x$thresholds[1] + 1), 
                          rep("Indirect type I", x$thresholds[2] - x$thresholds[1]), 
                          rep("Direct type I", x$n - x$thresholds[2])),
                   p_a = probs_alt,
                   tII = c(rep("Direct type II", x$thresholds[1] + 1), 
                          rep("Indirect type II", x$thresholds[2] - x$thresholds[1]), 
                          rep("None", x$n - x$thresholds[2])))
  
  ggplot2::ggplot(df, aes(x = y)) + 
    ggplot2::geom_bar(aes(y = p_n, fill = tI, colour = tI), stat = "identity", alpha = 0.4) + 
    ggplot2::geom_bar(aes(y = probs_alt, fill = tII, colour = tII), stat = "identity", alpha = 0.4) +
    ggplot2::scale_fill_manual(values = c("red", "darkgreen", "orange",  "green", "gray70"), 
                      name = "Error") +
    ggplot2::scale_colour_manual(values = c("red", "darkgreen", "orange",  "green", "gray70"), 
                               name = "Error") +
    ggplot2::geom_vline(xintercept = x$thresholds[1], linetype = 2) +
    ggplot2::geom_vline(xintercept = x$thresholds[2], linetype = 2) +
    ggplot2::ylab("Probability") + ggplot2::xlab("Outcome") + 
    ggplot2::theme_minimal()
}


min_x_1_bin <- function(n, rho_0, alpha_nom, tau_min, eta){
  if(eta <= alpha_nom){
    stop("The probability of an error following in intermediate outcome should
         not be less than the nominal type I error rate.")
  } else {
    # For given n, find the minimum x_1 which can lead to a valid choice of
    # x_0 (i.e. one which will give alpha <= alpha_nom).
    stats::qbinom((1 - 1/eta + alpha_nom/eta)/(1 - 1/eta), n, rho_0 - tau_min)
  }
}

opt_x_0_bin <- function(x_1, n, rho_0, alpha_nom, tau_min, eta){
  # For given x_1 find the x_0 which best satisfies alpha_nom
  z <- 1/eta - alpha_nom/eta + (1 - 1/eta)*stats::pbinom(x_1, n, rho_0 - tau_min)
  x_0 <- stats::qbinom(z, n, rho_0 - tau_min)
  return(x_0)
}

check_arguments_bin <- function(rho_0, rho_1){
    if(rho_0 < 0 | rho_0 > 1){
      stop("Hypothesis rho_0 is outside the [0, 1] interval.")
    }
    if(rho_1 < 0 | rho_1 > 1){
      stop("Hypothesis rho_1 is outside the [0, 1] interval.")
    }
}
