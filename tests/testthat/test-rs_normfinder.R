test_that("Normfinder online script can be loaded", {

  set.seed(100)
  ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  options("HTTPUserAgent" = "RStudio Desktop (2022.7.1.554)")
  write(options("HTTPUserAgent")[[1]], "~/R/testlog.txt", append = TRUE)
  res <- rs_normfinder(ct_vals)

  is.data.frame(res)

  expect_true(is.data.frame(res))

})




