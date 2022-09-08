

test_that("imports works for csv", {
  ## rs_loadtxtdata

    loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/csvtest/FFPE.csv") )

    # Create refference file
    # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

    expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})

test_that("imports works for tsv", {
  ## rs_loadtxtdata

  loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/tsvtest/FFPE.tsv") )

  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})


test_that("imports works for txt", {
  ## rs_loadtxtdata

  loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/txttest/FFPE.txt") )

  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})


test_that("imports works for excel", {
  ## rs_loadtxtdata


  loadeddata <- rs_loadexceldata ( paste0(testrefpath(), "/exceltest/Reffinder_data_test.xlsx") )

  # Create refference file
   # writeLines(capture.output(loadeddata[[1]]), paste0(testrefpath(), "/testref/rs_loadexceldata1.txt"))
   # writeLines(capture.output(loadeddata[[2]]), paste0(testrefpath(), "/testref/rs_loadexceldata2.txt"))
   # writeLines(capture.output(loadeddata[[3]]), paste0(testrefpath(), "/testref/rs_loadexceldata3.txt"))

  expect_equal(capture.output(loadeddata[[1]]), readLines(paste0(testrefpath(), "/testref/rs_loadexceldata1.txt")) )
  expect_equal(capture.output(loadeddata[[2]]), readLines(paste0(testrefpath(), "/testref/rs_loadexceldata2.txt")) )
  expect_equal(capture.output(loadeddata[[3]]), readLines(paste0(testrefpath(), "/testref/rs_loadexceldata3.txt")) )

})

