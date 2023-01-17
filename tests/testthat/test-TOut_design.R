test_that("Lower constraints increase sample size", {
  # Alpha
  expect_gt(TOut_design_bin(0.5, 0.7, 0.02, 0.1, 0.5)[1] - 
              TOut_design_bin(0.5, 0.7, 0.05, 0.1, 0.5)[1], 0)
  # Beta
  expect_gt(TOut_design_bin(0.5, 0.7, 0.05, 0.05, 0.5)[1] - 
              TOut_design_bin(0.5, 0.7, 0.05, 0.1, 0.5)[1], 0)
  # Gamma
  expect_gt(TOut_design_bin(0.5, 0.7, 0.05, 0.1, 0.3)[1] - 
              TOut_design_bin(0.5, 0.7, 0.05, 0.1, 0.5)[1], 0)
})


test_that("Lower nominal gamma increases intermediate zone", {

  d1 <- TOut_design_bin(0.5, 0.7, 0.05, 0.1, 0.3)
  
  d2 <- TOut_design_bin(0.5, 0.7, 0.05, 0.1, 0.5)
  
  expect_gt((d1[3] - d1[2]) - (d2[3] - d2[2]), 0)
})

# Add tests to check that a gamma = 1 constraint leads to standard two-outcome
# designs (at least approximately)
