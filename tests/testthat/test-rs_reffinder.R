


test_that("reffinder works with data.frames", {

  set.seed(100)
  ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  res <- rs_reffinder(ct_vals)

  write(getwd(), "~/R/testlog.txt", append = TRUE)


  expect_equal(res[[1]], read.csv(paste0(testrefpath(), "/rs_reffinder_df1.csv"), check.names = FALSE))
  expect_equal(res[[2]], read.csv(paste0(testrefpath(), "/rs_reffinder_df2.csv"), check.names = FALSE))

})


test_that("reffinder works with matrices", {

  set.seed(100)
  ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  res <- rs_reffinder(ct_vals)

  write(getwd(), "~/R/testlog.txt", append = TRUE)


  expect_equal(res[[1]], read.csv(paste0(testrefpath(), "/rs_reffinder_df1.csv"), check.names = FALSE))
  expect_equal(res[[2]], read.csv(paste0(testrefpath(), "/rs_reffinder_df2.csv"), check.names = FALSE))

})


test_that("reffinder works with tibble", {

  set.seed(100)
  tb_vals <- tibble::as_tibble(as.data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)))
  names(tb_vals) <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  tb_res <- rs_reffinder(tb_vals)

  expect_equal(tb_res[[1]], read.csv(paste0(testrefpath(), "/rs_reffinder_df1.csv"), check.names = FALSE))
  expect_equal(tb_res[[2]], read.csv(paste0(testrefpath(), "/rs_reffinder_df2.csv"), check.names = FALSE))

})




