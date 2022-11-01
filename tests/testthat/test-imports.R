


####################################################

## Test for deprecated functions

test_that("imports works for csv", {
## rs_loadtxtdata
  suppressWarnings(
    loadeddata <- rsloadtable( paste0(testrefpath(),"/csvtest/", list.files(paste0(testrefpath(),"/csvtest" ), pattern = "*.csv")) )
  )

  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})

test_that("imports works for tsv", {
  ## rs_loadtxtdata

  #loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/tsvtest/FFPE.tsv") )
  suppressWarnings(
    loadeddata <- rsloadtable( paste0(testrefpath(),"/tsvtest/", list.files(paste0(testrefpath(),"/tsvtest" ), pattern = "*.tsv")) )
  )

  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})


test_that("imports works for txt", {
  ## rs_loadtxtdata
  suppressWarnings(
  #loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/txttest/FFPE.txt") )
    loadeddata <- rsloadtable( paste0(testrefpath(),"/txttest/", list.files(paste0(testrefpath(),"/txttest" ), pattern = "*.txt")) )
  )
  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})
#########################################################


test_that("imports works for csv", {
  ## rs_loadtxtdata
  #suppressWarnings(
    loadeddata <- rsloadtable( paste0(testrefpath(),"/csvtest/", list.files(paste0(testrefpath(),"/csvtest" ), pattern = "*.csv")) )
  #)

  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})


test_that("imports works for tsv", {
  ## rs_loadtxtdata

  #loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/tsvtest/FFPE.tsv") )
  suppressWarnings(
    loadeddata <- rsloadtable( paste0(testrefpath(),"/tsvtest/", list.files(paste0(testrefpath(),"/tsvtest" ), pattern = "*.tsv")) )
  )

  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})


test_that("imports works for txt", {
  ## rs_loadtxtdata
  suppressWarnings(
    #loadeddata <- rs_loadtxtdata( paste0(testrefpath(), "/txttest/FFPE.txt") )
    loadeddata <- rsloadtable( paste0(testrefpath(),"/txttest/", list.files(paste0(testrefpath(),"/txttest" ), pattern = "*.txt")) )
  )
  # Create refference file
  # writeLines(capture.output(loadeddata), paste0(testrefpath(), "/testref/rs_loadtxtdata.txt"))

  expect_equal(capture.output(loadeddata), readLines(paste0(testrefpath(), "/testref/rs_loadtxtdata.txt")) )

})





#########################################################
test_that("imports works for excel", {
  ## rs_loadtxtdata


  loadeddata <- rsloadspreadsheet ( paste0(testrefpath(), "/excel-ods-test/Reffinder_data_test.xlsx") )

  # Create refference file
   # writeLines(capture.output(loadeddata[[1]]), paste0(testrefpath(), "/testref/rs_loadexceldata1.txt"))
   # writeLines(capture.output(loadeddata[[2]]), paste0(testrefpath(), "/testref/rs_loadexceldata2.txt"))
   # writeLines(capture.output(loadeddata[[3]]), paste0(testrefpath(), "/testref/rs_loadexceldata3.txt"))

  expect_equal(capture.output(loadeddata[[1]]), readLines(paste0(testrefpath(), "/testref/rs_loadexceldata1.txt")) )
  expect_equal(capture.output(loadeddata[[2]]), readLines(paste0(testrefpath(), "/testref/rs_loadexceldata2.txt")) )
  expect_equal(capture.output(loadeddata[[3]]), readLines(paste0(testrefpath(), "/testref/rs_loadexceldata3.txt")) )

})


test_that("imports works for ods", {
  ## rs_loadtxtdata

  loadeddata <- rsloadspreadsheet ( paste0(testrefpath(), "/excel-ods-test/Reffinder_data_test.ods") )

  # Create refference file
  # writeLines(capture.output(loadeddata[[1]]), paste0(testrefpath(), "/testref/rs_loadodsdata1.txt"))
  # writeLines(capture.output(loadeddata[[2]]), paste0(testrefpath(), "/testref/rs_loadodsdata2.txt"))
  # writeLines(capture.output(loadeddata[[3]]), paste0(testrefpath(), "/testref/rs_loadodsdata3.txt"))

  expect_equal(capture.output(loadeddata[[1]]), readLines(paste0(testrefpath(), "/testref/rs_loadodsdata1.txt")) )
  expect_equal(capture.output(loadeddata[[2]]), readLines(paste0(testrefpath(), "/testref/rs_loadodsdata2.txt")) )
  expect_equal(capture.output(loadeddata[[3]]), readLines(paste0(testrefpath(), "/testref/rs_loadodsdata3.txt")) )

})



