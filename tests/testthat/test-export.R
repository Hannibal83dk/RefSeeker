



test_that("export as excel file", {


  set.seed(100)
  ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
  dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

  res <- rs_reffinder(ct_vals)


  # rs_exceltable(res, paste0(testrefpath(), "/testref/rs_exceltable"), addDate = FALSE)

  capture.output(rs_exceltable(res, paste0(testrefpath(), "/excelexporttest"), addDate = FALSE))


  expect_equal(readxl::read_excel(paste0(testrefpath(), "/excelexporttest.xlsx"), 1),
               readxl::read_excel(paste0(testrefpath(), "/testref/rs_exceltable.xlsx"), 1)
               )

  expect_equal(readxl::read_excel(paste0(testrefpath(), "/excelexporttest.xlsx"), 2),
               readxl::read_excel(paste0(testrefpath(), "/testref/rs_exceltable.xlsx"), 2)
               )


  unlink(paste0(testrefpath(), "/excelexporttest.xlsx"     ) )


})








