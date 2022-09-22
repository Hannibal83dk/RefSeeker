


#' Load one or more datasets from an excel file
#'
#' @param filepath a path to an excel or ods file. If left blank, a dialog will appear to select this interactivly.
#' @return A list containing expression data for each sheet in the excel file
#'
#' @import readxl
#' @import readODS
#'
#' @export
#'
#' @aliases rs_loadodsdata()
#'
#' @examples
#'
#' \dontrun{
#' rs_loadexceldata()
#' }
#'
#'
rs_loadexceldata <- function(filepath = ""){

  datalist <- list()

  if(filepath == ""){
    filepath <- file.choose()
  }

  ext <- tolower(tools::file_ext(filepath))

# For excel files
  if(substring(ext, 1,2) == "xl"){

      sheets <- readxl::excel_sheets(filepath)

    for (i in 1:length(sheets)){

      datalist[[i]] <- readxl::read_excel(filepath, i)

    }

    names(datalist) <- sub(" ", "_", sheets)
  }

# For ods
  if(ext == "ods"){

    sheets <- readODS::list_ods_sheets(filepath)

    for (i in 1:length(sheets)){

      datalist[[i]] <- readODS::read_ods(filepath, i)

    }

    names(datalist) <- sub(" ", "_", sheets)
  }
  
  
  
  
# Testing for missing values
  for (i in 1:length(datalist)) {
    #datalist[[i]][5,2] <- NA
    
    if(FALSE %in% complete.cases(datalist[[i]])){
      warning(paste("Caution: Missing data located in ", names(datalist[i])))

    }
    
  }

  # Testing columns for outliers. More than 20% is may be indicative of transposed data table.
  ## Note this test is not based on anything other than the authers idea of how to test for columns not representing targets
  
  caution = FALSE
  for (i in 1:length(datalist)) {
    for (j in 1:ncol(datalist[[i]])) {
      
      # cat(paste("\nTesting: [", names(datalist[i]), "-", names(datalist[[i]])[j],"]"))
      box <- boxplot(datalist[[i]][j])
      
      if(length(box$out)/box$n > 0.2){
        #cat(" - Caution, more than 20% outliers detected")
        caution <- TRUE
      } else {#cat("- OK")
        }

    }
    
  }
  
  if(caution){
    
    warning("Caution, more than 20% outliers were detected in some colums.
            \rPlease check that targets(Gene/mRNA/miRNA) are represented as columns and samples as rows")
  }
  
  #cat("\nPlease note: These tests are not a guarantee for (im)prober structure of dataset(s)")

  return(datalist)
}


#' @rdname rs_loadexceldata
#' @examples
#' \dontrun{
#' rs_loadodsdata()
#' }
#' @export
rs_loadodsdata <- rs_loadexceldata



  



