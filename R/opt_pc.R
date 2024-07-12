# Find optimal progression criteria for a fixed sample size

opt_pc <- function(n, rho_0, rho_1, alpha_nom, beta_nom, 
                       tau = c(0,0), eta_0 = 0.5, eta_1 = NULL, x = NULL, sigma = NULL){
  
  tau_min <- tau[1]
  tau_max <- tau[2]
  
  if(is.null(eta_1)) eta_1 <- eta_0
  
  # Check that the arguments are specified correctly
  check_arguments(n, rho_0, rho_1, alpha_nom, beta_nom, eta_0, sigma)
  
  if(n <= 0) {
    if(is.null(sigma)) {
      return(null_design(n, rho_0, rho_1, tau, eta_0, eta_1))
    } else {
      return(null_design(n, rho_0, rho_1, tau, eta_0, eta_1, sigma))
    }
  }
  
  if(!is.null(x)){
    x_0 <- x[1]; x_1 <- x[2]
    if(is.null(sigma)) {
      ocs <- get_ocs_bin(n, x_0, x_1, rho_0, rho_1, tau_min, tau_max, eta_0, eta_1)
    } else {
      ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, tau_min, tau_max, eta_0, eta_1)
    }
  } else {
    
    if(is.null(sigma)) {
      
      # Get minimum x_1 s.t. alpha can be controlled, and default max x_1
      min_x_1 <- min_x_1_bin(n, rho_0, alpha_nom, tau_min, eta_0)
      max_x_1 <- n
      if(max_x_1 < min_x_1) return(null_design(n, rho_0, rho_1, tau, eta_0, eta_1))
      
      # Find optimal choice of x_1 - that which gives beta ~ beta_nom
      # Run an exhaustive search over all possible choices of x_1
      x_1s <- min_x_1:max_x_1
      # For each, find the optimal x_0
      x_0s <- opt_x_0_bin(x_1s, n, rho_0, alpha_nom, tau_min, eta_0)
      # Get resulting betas
      betas <- get_ocs_bin(n, x_0s, x_1s, rho_0, rho_1, tau_min, tau_max, eta_0, eta_1)[,2]
      # Convert betas to a measure to be minimised when optimising
      to_min <- beta_objective(betas, beta_nom, x_0s, x_1s)
      x_0 <- x_0s[which.min(to_min)]
      x_1 <- x_1s[which.min(to_min)]
      
      ocs <- get_ocs_bin(n, x_0, x_1, rho_0, rho_1, tau_min, tau_max, eta_0, eta_1)
    } else {
      
      # Get minimum x_1 s.t. alpha can be controlled, and default max x_1
      min_x_1 <- min_x_1_cont(alpha_nom)
      max_x_1 <- max_x_1_cont(rho_0, rho_1, sigma, n)
      if(max_x_1 < min_x_1) return(null_design(n, rho_0, rho_1, tau, eta_0, eta_1, sigma))
      
      # Find optimal choice of x_1 - this will be the largest value such that
      # beta ~ beta_nom
      # Use optimise() to find the best choice of x_1
      x_1 <- stats::optimize(opt_x_1_cont, lower = min_x_1, upper = max_x_1,
                             n=n, rho_0=rho_0, rho_1=rho_1, sigma=sigma, 
                             alpha_nom=alpha_nom, beta_nom=beta_nom, 
                             tau_min=tau_min, tau_max=tau_max, eta_0=eta_0, eta_1=eta_1)$minimum
      
      # Determine the corresponding x_0 to give alpha = alpha_nom
      x_0 <- opt_x_0_cont(x_1, n, sigma, alpha_nom, tau_min, eta_0)
      
      ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, tau_min, tau_max, eta_0, eta_1)
    }
  }
  
  # Check if all constraints are (approximately) satisfied
  valid <- FALSE
  if(all(ocs[1:2] < (c(alpha_nom, beta_nom) + 0.001))){
    valid <- TRUE
  }
  
  new_tout(valid, n, x_0, x_1, ocs[1], ocs[2], ocs[3], rho_0, rho_1, tau, eta_0, eta_1, sigma)
}

null_design <- function(n, rho_0, rho_1, tau, eta_0, eta_1, sigma = NULL){
  
  new_tout(FALSE, n, NA, NA, NA, NA, NA, rho_0, rho_1, tau, eta_0, eta_1, sigma)
}

check_arguments <- function(n, rho_0, rho_1, alpha_nom, beta_nom, eta_0, sigma){
  
  if(n < 0){
    stop("Sample size n is negative.")
  }
  
  if(alpha_nom < 0 | alpha_nom > 1){
    stop("Contraint alpha_nom is outside the [0, 1] interval.")
  }
  
  if(beta_nom < 0 | beta_nom > 1){
    stop("Contraint beta_nom is outside the [0, 1] interval.")
  }
  
  if(eta_0 < 0 | eta_0 > 1){
    stop("Probability eta_0 is outside the [0, 1] interval.")
  }
  
  if(!is.null(sigma)){
    if(sigma < 0){
      stop("Standard deviation sigma is negative.")
    }
  } else {
    if(rho_0 < 0 | rho_0 > 1){
      stop("Hypothesis rho_0 is outside the [0, 1] interval.")
    }
    if(rho_1 < 0 | rho_1 > 1){
      stop("Hypothesis rho_1 is outside the [0, 1] interval.")
    }
  }
}
