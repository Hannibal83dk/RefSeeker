
library(refSeeker)
xldatapath <- "/home/patrick/OneDrive/Dokumenter/R/reffindertester2/GSE73581_test_RF.txt"
xldata <- read.table(xldatapath, header = TRUE)

timings <- matrix(ncol = 100, nrow = 100) # col = number of targets, row = number of samples


n = 100


for (i in 41:43) {
  for (j in 3:n) {

    set <- xldata[1:i, 1:j]

    start <- Sys.time()
    capture.output( rs_reffinder(set) )
    dur <- difftime(Sys.time(), start, units = "secs")

    timings[i,j] <- dur

    write.table(timings, "refSeekertimings")



    nhash <- floor((j/n)*50)
    nspace <- 50-nhash

    cat(paste0( "\r[",
                paste(replicate(nhash, "#"), collapse = ""),
                paste(replicate(nspace, " "), collapse = ""),
                "] ",
                i, " of ", n
    ))



  }


}





rs_reffinder(set1)
















