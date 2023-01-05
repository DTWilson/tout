test_that("Infeasible inputs generate null designs", {
  # Zero sample size
  expect_null(opt_pc(n = 0, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2, gamma_nom = 0.9))
  # Null larger than alternative
  expect_null(opt_pc(n = 200, rho_0 = 0.8, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2, gamma_nom = 0.9))
  # 0 nominal alpha
  expect_null(opt_pc(n = 200, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0, beta_nom = 0.2, gamma_nom = 0.9))
  # 0 nominal beta
  expect_null(opt_pc(n = 200, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0, gamma_nom = 0.9))
  # 0 nominal gamma
  expect_null(opt_pc(n = 200, rho_0 = 0.5, rho_1 = 0.7,
                     alpha_nom = 0.05, beta_nom = 0.2, gamma_nom = 0))
})
