



## - Make sure the package installs and work properly
## - Check the package

# unlink("vignettes/figure", recursive = T)

devtools::check()

## - Make sure the orig vignettes can be knittet succesfully 



setwd("./vignettes/")
knitr::knit("reffindeR-intro.Rmd.orig", output = "reffindeR-intro.Rmd")
setwd("../")

## - Verify that the output is functional
## 	- Open the .Rmd file in R and knit
##	- Check output for run errors or missing figures
## If all i OK build the vignette

devtools::build_vignettes()


devtools::document(roclets = c('rd', 'collate', 'namespace'))


devtools::build()

detach("package:reffindeR", unload = TRUE)


install.packages("F:/reffindeR_0.9.2.tar.gz", repos = NULL, type = "source")
