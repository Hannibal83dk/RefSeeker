test_that("deltaCt on matrix", {
  set.seed(100)
  ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  Target <- c("gene2", "gene1", "gene4", "gene3", "gene5")

  "Average of STDEV" <- c(1.292, 1.295, 1.402, 1.453, 1.614)

  "Stability Rank" <- c(1,2,3,4,5)

  results <- data.frame(Target, `Average of STDEV`, `Stability Rank`, check.names = F)

  expect_equal(rs_deltaCt(ct_vals), results)
})


test_that("deltaCt on data.frame", {
  set.seed(100)
  ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  Target <- c("gene2", "gene1", "gene4", "gene3", "gene5")

  "Average of STDEV" <- c(1.292, 1.295, 1.402, 1.453, 1.614)

  "Stability Rank" <- c(1,2,3,4,5)

  results <- data.frame(Target, `Average of STDEV`, `Stability Rank`, check.names = F)

  expect_equal(rs_deltaCt(ct_vals), results)
})
