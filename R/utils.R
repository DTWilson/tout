# Take beta and convert into an objective function to minimise, penalising
# constraint violations.
beta_objective <- function(beta, beta_nom, x_0, x_1){
  # Take absolute value to minimise
  beta2 <- abs(beta - beta_nom)
  # Find largest x_1 while penalising (i) beta constrain violation, 
  # and (ii) cases where x_0 > x_1
  return(-x_1 + 100000*(beta > beta_nom)*(beta - beta_nom) + (x_0 > x_1)*1000)
}