test_that("OCs change with inputs sensibly (binary)", {
  # Increasing x_0 and x_1
  ocs1 <- get_ocs_bin(n=100, x_0=50, x_1=60, rho_0=0.5, rho_1=0.5, tau_min=0, tau_max=0) 
  ocs2 <- get_ocs_bin(n=100, x_0=60, x_1=70, rho_0=0.5, rho_1=0.5, tau_min=0, tau_max=0)
  
  expect_gte(ocs1[1] - ocs2[1], 0)
  expect_gte(ocs2[2] - ocs1[2], 0)
  
  # Increasing distance between x_0 and x_1
  ocs3 <- get_ocs_bin(n=100, x_0=45, x_1=65, rho_0=0.5, rho_1=0.5, tau_min=0, tau_max=0) 
  
  expect_gte(ocs1[3] - ocs3[3], 0)
})

test_that("OCs change with inputs sensibly (continuous)", {
  # Increasing x_0 and x_1
  ocs1 <- get_ocs_cont_z(n=100, x_0=1.3, x_1=1.9, rho_0=0, rho_1=0.3, sigma=1, tau_min=0, tau_max=0) 
  ocs2 <- get_ocs_cont_z(n=100, x_0=1.5, x_1=2.1, rho_0=0, rho_1=0.3, sigma=1, tau_min=0, tau_max=0) 
  
  expect_gte(ocs1[1] - ocs2[1], 0)
  expect_gte(ocs2[2] - ocs1[2], 0)
  
  # Increasing distance between x_0 and x_1
  ocs3 <- get_ocs_cont_z(n=100, x_0=1.2, x_1=2, rho_0=0, rho_1=0.3, sigma=1, tau_min=0, tau_max=0) 
  
   expect_gte(ocs1[3] - ocs3[3], 0)
})
