test_that("Lower constraints increase sample size (binary)", {
  # Alpha
  expect_gt(tout_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.02, beta_nom=0.1)$n - 
              tout_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.1)$n, 0)
  # Beta
  expect_gt(tout_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.05)$n - 
              tout_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.1)$n, 0)
})


