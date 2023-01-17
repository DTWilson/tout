# Take beta and convert into an objective function to minimise, penalising
# constraint violations.
beta_objective <- function(beta, beta_nom, x_0, x_1){
  # Take absolute value to minimise
  beta2 <- abs(beta - beta_nom)
  # Find largest x_1 while penalising (i) beta constrain violation, 
  # and (ii) cases where x_0 > x_1
  return(-x_1 + 1000*(beta > beta_nom) + (x_0 > x_1)*1000)
}


# Check common arguments and throw errors if invalid.
check_arguments <- function(n, alpha_nom, beta_nom, gamma_nom, eta){
  if(n < 0){
    stop("Sample size n is negative.")
  }
  if(alpha_nom < 0 | alpha_nom > 1){
    stop("Constraint alpha_nom is outside the [0, 1] interval.")
  }
  if(beta_nom < 0 | beta_nom > 1){
    stop("Constraint beta_nom is outside the [0, 1] interval.")
  }
  if(gamma_nom < 0 | gamma_nom > 1){
    stop("Constraint gamma_nom is outside the [0, 1] interval.")
  }
  if(eta < 0 | eta > 1){
    stop("Probability eta is outside the [0, 1] interval")
  }
}
