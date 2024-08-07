test_that("Infeasible inputs generate null designs (continuous)", {
  # Zero sample size
  expect_false(opt_pc(n = 0, rho_0 = 0.5, rho_1 = 0.7, sigma = 0.7,
                           alpha_nom = 0.05, beta_nom = 0.2, eta_0 = 0.5, eta_1 = 0.5)$valid)
  # Null larger than alternative
  expect_false(opt_pc(n = 100, rho_0 = 0.75, rho_1 = 0.7, sigma = 0.7,
                           alpha_nom = 0.05, beta_nom = 0.2, eta_0 = 0.5, eta_1 = 0.5)$valid)
  # 0 nominal beta
  expect_false(opt_pc(n = 100, rho_0 = 0.5, rho_1 = 0.7, sigma = 0.7,
                           alpha_nom = 0.05, beta_nom = 0, eta_0 = 0.5, eta_1 = 0.5)$valid)
})

test_that("Infeasible inputs generate null designs (binary)", {
  # Zero sample size
  expect_false(opt_pc(n = 0, rho_0 = 0.5, rho_1 = 0.7,
                          alpha_nom = 0.05, beta_nom = 0.2)$valid)
  # Null larger than alternative
  expect_false(opt_pc(n = 100, rho_0 = 0.75, rho_1 = 0.7,
                          alpha_nom = 0.05, beta_nom = 0.2)$valid)
  # 0 nominal alpha
  expect_false(opt_pc(n = 100, rho_0 = 0.5, rho_1 = 0.7,
                          alpha_nom = 0, beta_nom = 0.2)$valid)
  # 0 nominal beta
  expect_false(opt_pc(n = 100, rho_0 = 0.5, rho_1 = 0.7,
                          alpha_nom = 0.05, beta_nom = 0)$valid)
})