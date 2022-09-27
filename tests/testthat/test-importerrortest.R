test_that("BasicDataset format ", {
  expect_equal(2 * 2, 4)
})


test_that("missingDataIsCaughtWhenLoading", {

  expect_warning(
    rs_loadcsvdata("/home/patrick/OneDrive/Dokumenter/R/refSeeker/inst/exdata/csverrortest/Reffinder_Plasma_error_test.txt"),
    "Caution: Missing data located in: Reffinder_Plasma_error_test \nMissing data is discauraged and may lead to errors"
  )

})


# Test for non numeric columns. May indicate transposed data set, anyway errors will happen in this case
test_that("missingDataIsCaughtWhenLoading", {

  expect_warning(
    rs_loadcsvdata("/home/patrick/OneDrive/Dokumenter/R/refSeeker/inst/exdata/csverrortest/Reffinder_FFPE_error_test.csv"),
    "Non numeric columns found in: Reffinder_FFPE_error_test \nPlease make sure columns represent target RNA species and rows represent samples"
  )

})



test_that("missingDataIsCaughtWhenLoading", {

  expect_warning(
    expect_snapshot_warning(rs_loadexceldata("/home/patrick/OneDrive/Dokumenter/R/refSeeker/inst/exdata/excel-ods-test/Reffinder_data_error_test.ods"))
  )

})




