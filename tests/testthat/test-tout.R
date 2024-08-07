test_that("printing works (binary)", {
  x <- tout_design(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.1, gamma_nom = 0.9)
  expect_snapshot(x)
})
  
test_that("printing works (continuous)", {
  x <- tout_design(rho_0=0, rho_1=0.4, alpha_nom=0.05, beta_nom=0.1, gamma_nom = 0.6, sigma=1)
  expect_snapshot(x)
})


test_that("plotting works (binary)", {
  x <- tout_design(rho_0=0.5, rho_1=0.7, alpha_nom=0.05, beta_nom=0.1, gamma_nom = 0.9)
  vdiffr::expect_doppelganger("Binary tout plot", x)
})

test_that("plotting works (continuous)", {
  x <- tout_design(rho_0=0, rho_1=0.4, alpha_nom=0.05, beta_nom=0.1, gamma_nom = 0.6, sigma=1)
  vdiffr::expect_doppelganger("Continuous tout plot", x)
})

