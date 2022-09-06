
library(reffindeR)
## rf_loadcsvdata


test_rf_loadcsvdata <- function(){
  loadeddata <- rf_loadcsvdata("./inst/exdata/csvtest/FFPE.csv")

# Setup test comparison file
# capture.output(loadeddata, file ="./mantests/rf_loadcsvdata.txt")

  if(!any(FALSE) %in% (capture.output(loadeddata) == readLines("./mantests/rf_loadcsvdata.txt"))) {

    cat("rf_loadcsvdata test successful\n")
  } else { warning("rf_loadcsvdata test failed\n") }

}

###############################################

## rf_loadtxtdata

test_rf_loadtxtdata <- function(){
  loadeddata <- rf_loadtxtdata("./inst/exdata/csvtest/FFPE.csv")

  # Setup test comparison file
  # capture.output(loadeddata, file ="./mantests/rf_loadtxtdata.txt")

  ## csv-files
  if(!any(FALSE) %in% (capture.output(loadeddata) == readLines("./mantests/rf_loadtxtdata.txt"))) {

    cat("rf_loadtxtdata csv test successful\n")
  } else { warning("rf_loadtxtdata csv test failed\n") }

  ## tsv-files
  loadeddata <- rf_loadtxtdata("./inst/exdata/tsvtest/FFPE.tsv")

  if(!any(FALSE) %in% (capture.output(loadeddata) == readLines("./mantests/rf_loadtxtdata.txt"))) {

    cat("rf_loadtxtdata tsv test successful\n")
  } else { warning("rf_loadtxtdata tsv test failed\n") }

  ## txt-files mixed delimiter content
  loadeddata <- rf_loadtxtdata("./inst/exdata/txttest/FFPE.txt")

  if(!any(FALSE) %in% (capture.output(loadeddata) == readLines("./mantests/rf_loadtxtdata.txt"))) {

    cat("rf_loadtxtdata txt test successful\n")
  } else { warning("rf_loadtxtdata txt test failed\n") }


}


#######################################3

## test refference setup

testdata <- rf_loadcsvdata("./inst/exdata/csvtest/FFPE.csv")
testdata <- rf_reffinder(testdata$FFPE)
stability <- testdata$stabilityTable
rank <- testdata$rankTable

rfexcelfile(filename = "./mantests/rfexcelfiletestref", stability, rank, addFilter = T, overwrite = T)


test_rfexcelfile <- function(){

  testdata <- rf_loadcsvdata("./inst/exdata/csvtest/FFPE.csv")
  testdata <- rf_reffinder(testdata$FFPE)
  stability <- testdata$stabilityTable
  rank <- testdata$rankTable

  temp <- capture.output(rfexcelfile(filename = "./mantests/rfexcelfiletest", stability, rank, addFilter = T, overwrite = T))

  refstab <- openxlsx::read.xlsx("./mantests/rfexcelfiletestref.xlsx", sheet = 1)
  refrank <- openxlsx::read.xlsx("./mantests/rfexcelfiletestref.xlsx", sheet = 2)

  teststab <- openxlsx::read.xlsx("./mantests/rfexcelfiletest.xlsx", sheet = 1)
  testrank <- openxlsx::read.xlsx("./mantests/rfexcelfiletest.xlsx", sheet = 2)

  if(!any(FALSE) %in% (refstab == teststab)){
    if(!any(FALSE) %in% (refrank == testrank)){


      return(cat("rfexcelfile PASSED\n"))
    }
  }
  warning("test_rfexcelfile PASSED\n")


}
#############################################
# rf_exceltable

## test refference setup

testdata <- rf_loadcsvdata("./inst/exdata/csvtest/FFPE.csv")
testdata <- rf_reffinder(testdata$FFPE)

test_rf_exceltable <- function(){

  rf_exceltable(testdata, filename = "./mantests/rf_exceltabletest")

}

test_rf_all <- function(){
  test_rf_loadcsvdata()
  test_rf_loadtxtdata()
  test_rfexcelfile()
}


test_rf_all()







