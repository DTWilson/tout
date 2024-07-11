#' @export
print.tout_design_bin <- function(x, ... ){
    cat("Three-Outcome design (binary outcome)\n")
    cat("\n")
    cat("Sample size:", x$n, "\n")
    cat("Decision thresholds:", x$thresholds, "\n")
    cat("\n")
    cat("alpha =", x$alpha, "\nbeta =", x$beta, "\ngamma =", x$gamma, "\n")
    cat("\n")
    cat("Hypotheses:", x$hyps[1], "(null),", x$hyps[2], "(alternative)\n")
    cat("Modification effect range:", x$tau, "\n")
    cat("Error probability following an intermediate result:", x$eta, "\n")
    
    if(!x$valid){
      cat("\nNote: design is invalid (see error rates). Consider increasing ")
      cat("the maximum sample size (max_n).")
    }
}

#' @importFrom ggplot2 aes
#' @export
plot.tout_design_bin <- function(x, y, ...){
  # Vector of possible outcomes
  ys <- 0:x$n
  # Probabilities of these outcomes under the null and alternative hypotheses
  probs_null <- stats::dbinom(ys, size = x$n, prob = x$hyps[1])
  probs_alt <- stats::dbinom(ys, size = x$n, prob = x$hyps[2])
  
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


min_x_1_bin <- function(n, rho_0, alpha_nom, tau_min, eta_0){
  if(eta_0 <= alpha_nom){
    stop("The probability of an error following in intermediate outcome should
         not be less than the nominal type I error rate.")
  } else {
    # For given n, find the minimum x_1 which can lead to a valid choice of
    # x_0 (i.e. one which will give alpha <= alpha_nom).
    stats::qbinom((1 - 1/eta_0 + alpha_nom/eta_0)/(1 - 1/eta_0), n, rho_0 - tau_min)
  }
}

opt_x_0_bin <- function(x_1, n, rho_0, alpha_nom, tau_min, eta_0){
  # For given x_1 find the x_0 which best satisfies alpha_nom
  z <- 1/eta_0 - alpha_nom/eta_0 + (1 - 1/eta_0)*stats::pbinom(x_1, n, rho_0 - tau_min)
  x_0 <- stats::qbinom(z, n, rho_0 - tau_min)
  return(x_0)
}


