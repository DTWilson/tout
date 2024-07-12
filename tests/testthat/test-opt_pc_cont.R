test_that("lower thresholds control type I error", {
  
  x_0 <- opt_x_0_cont(x_1=1.8, n=70, sigma=0.5, alpha_nom=0.05, tau_min=0, eta_0=0.5)
  expect_lte(get_ocs_cont_z(n=70, x_0=x_0, x_1=1.8, rho_0=0.5, rho_1=0.7, sigma=0.5, tau_min=0, tau_max=0, eta_0=0.5, eta_1=0.5)[1], 0.050001)
  
  x_0 <- opt_x_0_cont(x_1=1.8, n=70, sigma=0.5, alpha_nom=0.05, tau_min=0.05, eta_0=0.4)
  expect_lte(get_ocs_cont_z(n=70, x_0=x_0, x_1=1.8, rho_0=0.5, rho_1=0.7, sigma=0.5, tau_min=0.05, tau_max=0.05, eta_0=0.4, eta_1=0.4)[1], 0.050001)
})

test_that("upper thresholds are feasible", {
  
  x_1 <- min_x_1_cont(alpha_nom=0.05)
  x_0 <- opt_x_0_cont(x_1=x_1, n=70, sigma=0.5, alpha_nom=0.05, tau_min=0, eta_0=0.5)
  expect_lte(get_ocs_cont_z(n=70, x_0=x_0, x_1=x_1, rho_0=0.5, rho_1=0.7, sigma=0.5, tau_min=0, tau_max=0, eta_0=0.5, eta_1=0.5)[1], 0.050001)
  
  
  x_1 <- min_x_1_cont(alpha_nom=0.05)
  x_0 <- opt_x_0_cont(x_1=x_1, n=70, sigma=0.5, alpha_nom=0.05, tau_min=0.05, eta_0=0.4)
  expect_lte(get_ocs_cont_z(n=70, x_0=x_0, x_1=x_1, rho_0=0.5, rho_1=0.7, sigma=0.5, tau_min=0.05, tau_max=0.06, eta_0=0.4, eta_1=0.4)[1], 0.050001)
  
})

