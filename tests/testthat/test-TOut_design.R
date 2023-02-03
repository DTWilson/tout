test_that("Lower constraints increase sample size", {
  # Alpha
  expect_gt(TOut_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.02, beta_nom=0.1)[1] - 
              TOut_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.1)[1], 0)
  # Beta
  expect_gt(TOut_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.05)[1] - 
              TOut_design_bin(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.1)[1], 0)
})


