



test_that("missingDataIsCaughtWhenLoading", {

  expect_warning(



    rs_loaddata( paste0(testrefpath(), "/csverrortest/Reffinder_Plasma_error_test.txt") ),
    "Caution: Missing data located in: Reffinder_Plasma_error_test \nMissing data is discauraged and may lead to errors"
  )

})


# Test for non numeric columns. May indicate transposed data set, anyway errors will happen in this case
test_that("missingDataIsCaughtWhenLoading", {

  expect_warning(
    rsloadtable(paste0(testrefpath(), "/csverrortest/Reffinder_FFPE_error_test.csv")),
    "Non numeric columns found in: Reffinder_FFPE_error_test \nPlease make sure columns represent target RNA species and rows represent samples"
  )

})



test_that("missingDataIsCaughtWhenLoading", {

  expect_warning(
    expect_snapshot_warning(rsloadspreadsheet(paste0(testrefpath(), "/excel-ods-test/Reffinder_data_error_test.ods")))
  )

})




