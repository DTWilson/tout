new_tout <- function(valid, n, x_0, x_1, alpha, beta, gamma, rho_0, rho_1, tau, eta_0, eta_1, sigma) {

  structure(list(valid = valid, 
            n = n, thresholds = c(x_0, x_1), 
            alpha = alpha, beta = beta, gamma = gamma,
            hyps = c(rho_0, rho_1), sigma = sigma, tau = tau, eta = c(eta_0, eta_1)),
            class = "tout")
}

# Don't require a helper as users will not construct tout objects themselves

#' @export
print.tout <- function(x, ...){
  cat("Three-outcome design\n")
  cat("\n")
  cat("Sample size:", x$n, "\n")
  cat("Decision thresholds:", x$thresholds, "\n")
  cat("\n")
  cat("alpha =", x$alpha, "\nbeta =", x$beta, "\ngamma =", x$gamma, "\n")
  cat("\n")
  cat("Hypotheses:", x$hyps[1], "(null),", x$hyps[2], "(alternative)\n")
  if(!is.null(x$sigma)) cat("Standard deviation:", x$sigma, "\n")
  cat("Modification effect range:", x$tau, "\n")
  cat("Error probability following an intermediate result:", x$eta, "\n")
  
  if(!x$valid){
    cat("\nNote: design is invalid (see error rates). Consider increasing ")
    cat("the maximum sample size (max_n).")
  }
}

#' @importFrom ggplot2 aes
#' @export
plot.tout <- function(x, y, ...){
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