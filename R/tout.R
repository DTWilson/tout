new_tout <- function(valid, n, x_0, x_1, alpha, beta, gamma, rho_0, rho_1, tau, eta_0, eta_1, sigma) {

  structure(list(valid = valid, 
            n = n, thresholds = c(x_0, x_1), 
            alpha = alpha, beta = beta, gamma = gamma,
            hyps = c(rho_0, rho_1), sigma = sigma, tau = tau, eta = c(eta_0, eta_1)),
            class = "tout")
}

validate_tout <- function() {
  
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
