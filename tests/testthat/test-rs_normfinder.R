test_that("Normfinder online script can be loaded", {

  set.seed(100)
  ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  #options("HTTPUserAgent" = "RStudio Desktop (2022.7.1.554)")

  # Testing purpose
  ## During test and check the user agent is changed to cause a server "406 not acceptable" error
  #write(options("HTTPUserAgent")[[1]], "~/R/testlog.txt", append = TRUE)

  #write(find.package("refSeeker", lib.loc=NULL, quiet = TRUE), "~/R/testlog.txt", append = TRUE)


  res <- rs_normfinder(ct_vals)

  #expect_equal (res, read.csv("../../refSeeker/exdata/testref/rs_normfindertest1.csv", check.names=FALSE))

  expect_equal(res, read.csv(paste0(testrefpath(), "/rs_normfindertest1.csv"), check.names = FALSE))


})



