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

#' @export
plot.tout <- function(x, y, ...){
  
  if(is.null(x$sigma)){
    # Binary case
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
    
    df <- df[df$p_n > 0.00001 | df$p_a > 0.00001,]
    
    graphics::barplot(names=df$y, height=df$p_n,
            col=ifelse(df$tI == "Direct type I", t_col("darkgreen"),
                       ifelse(df$tI == "Indirect type I", t_col("orange"), t_col("gray70", 100))),
            main = expression(paste("Sampling distributions under null (", rho[0], ") and alternative (", rho[1], ") hypotheses")))
    
    graphics::par(new = TRUE)
    
    graphics::barplot(names=df$y, height=df$p_a,  yaxt = "n",
            col=ifelse(df$tII == "Direct type II", t_col("red"),
                       ifelse(df$tII == "Indirect type II", t_col("orange"), t_col("gray70", 100))))
    
    graphics::legend("topright", 
           legend = c("Stop", "Go", "Pause"), 
           col = c(t_col("red"), t_col("darkgreen"), t_col("orange")),
           pch = 15,
           bty = "n", 
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.0, 0.0))
    
  } else {
    # Continuous case
    ncp <- sqrt(x$n)*(x$hyps[2] - x$hyps[1])/x$sigma
    # Vector of possible outcomes
    ys <- seq(-3, ncp + 3, 0.01)
    # Probabilities of these outcomes under the null and alternative hypotheses
    probs_null <- stats::dnorm(ys, mean = 0, sd = 1)
    probs_alt <- stats::dnorm(ys, mean = ncp, 1)
    
    # Data frame for plotting, flagging all direct and indirect errors
    df <- data.frame(y = ys,
                     p_n = probs_null,
                     p_a = probs_alt)
    df$tI <- ifelse(df$y <= x$thresholds[1], "None",
                    ifelse(df$y >= x$thresholds[2], "Direct type I", "Indirect type I"))
    df$tII <- ifelse(df$y >= x$thresholds[2], "None",
                    ifelse(df$y <= x$thresholds[1], "Direct type II", "Indirect type II"))
    
    df <- df[df$p_n > 0.00001 | df$p_a > 0.00001,]
    
    graphics::plot(df$y, df$p_n, type = "l",  lwd=1, ylab='', xlab='',
         main = expression(paste("Sampling distributions under null (", rho[0], ") and alternative (", rho[1], ") hypotheses")))
    
    graphics::polygon(x = c(df$y[df$tI == "Direct type I"], max(df$y), x$thresholds[2], x$thresholds[2]),
            y = c(df$p_n[df$tI == "Direct type I"], 0, 0, stats::dnorm(x$thresholds[2], mean = 0, sd = 1)),
            col = t_col("darkgreen"), lty=0)
    
    graphics::polygon(x = c(df$y[df$tI == "Indirect type I"], x$thresholds[2], x$thresholds[1], x$thresholds[1]),
            y = c(df$p_n[df$tI == "Indirect type I"], 0, 0, stats::dnorm(x$thresholds[1], mean = 0, sd = 1)),
            col = t_col("orange"), lty=0)

    graphics::par(new = TRUE)
    
    graphics::plot(df$y, df$p_a, type = "l", lwd=1, ylab='', xlab='')
    
    graphics::polygon(x = c(min(df$y), df$y[df$tII == "Direct type II"], x$thresholds[1], x$thresholds[1]),
            y = c(0, df$p_a[df$tII == "Direct type II"], stats::dnorm(x$thresholds[1], mean = ncp, sd = 1), 0),
            col = t_col("red"), lty=0)
    
    graphics::polygon(x = c(x$thresholds[1], df$y[df$tII == "Indirect type II"], x$thresholds[2], x$thresholds[2], x$thresholds[1]),
            y = c(stats::dnorm(x$thresholds[1], mean = ncp, sd = 1), df$p_a[df$tII == "Indirect type II"], 
                  stats::dnorm(x$thresholds[2], mean = ncp, sd = 1), 0, 0),
            col = t_col("orange"), lty=0)

    graphics::legend("topright", 
           legend = c("Stop", "Go", "Pause"), 
           col = c(t_col("red"), t_col("darkgreen"), t_col("orange")),
           pch = 15,
           bty = "n", 
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.0, 0.0))
  }
}

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- grDevices::col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- grDevices::rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}