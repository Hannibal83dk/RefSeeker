# Normfinder2=function(filename,Groups=FALSE,ctVal=TRUE,pStabLim=0.25){
#   #
#   # If Groups is TRUE the last row contains the group identifier,
#   # and the last row must be started by a name for that row.
#   # No spaces are allowed in the gene names, sample names and group identifier.
#   #
#   dat0=expression
#   #
#   ntotal=dim(dat0)[2] # number of samples
#   k0=dim(dat0)[1] # number of rows
#   #
#
#   ngenes=k0 # number of genes
#   genenames=rownames(dat0)
#   grId=rep(1,ntotal)
#   #
#   dat=matrix(as.numeric(unlist(dat0)),ngenes,ntotal) # matrix instead of list
#   #
#   #if (!ctVal){dat=log2(dat)} # transform to log2 values
#   #
#   samplenames=colnames(dat0)
#   grId=factor(unlist(grId))  # group identifier
#   groupnames=levels(grId)  # group names
#   ngr=length(levels(grId)) # number of groups
#   # Number of samples in each group:
#   nsamples=rep(0,ngr)
#   for (group in 1:ngr){nsamples[group]=sum(grId==groupnames[group])}
#   #
#   #
#
#   da <- dat
#
#   MakeStabOne=function(da){
#     ngenes=dim(da)[1]
#     # Sample averages
#     sampleavg=apply(da,2,mean)
#     # Gene averages
#     geneavg=apply(da,1,mean)
#     totalavg=mean(da)
#     #
#     # Variances
#     genevar0=rep(0,ngenes)
#     for (gene in 1:ngenes){
#       genevar0[gene]=       sum(     (dat[gene,]-geneavg[gene]-sampleavg+totalavg)^2    )/
#                                               ( (ntotal-1)*(1-2/ngenes) )
#     }
#     genevar= genevar0-sum(genevar0) / (ngenes*ngenes-ngenes)
#     #
#     # Change possible negative values
#     geneMinvar=rep(0,ngenes)
#     z=da
#     for (gene in 1:ngenes){
#       varpair=rep(0,ngenes)
#       for (gene1 in 1:ngenes){varpair[gene1]=var(z[gene,]-z[gene1,])}
#       geneMinvar[gene]=min(varpair[-gene])/4
#     }
#     # Final variances
#     genevar=ifelse(genevar<0,geneMinvar,genevar)
#     #
#     return(genevar)
#   }
#   #     End of function MakeStabOne
#   #
#   #################################################
#   #
#   # Main part
#   #
#
#     #
#     sigma=sqrt(MakeStabOne(dat))
#     #
#
#     return(sigma)
#     #
#
#   #
# } # End of main function
#
#
#
#
# set.seed(100)
# ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
# dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
# ct_vals
#
# library(reffindeR)
#
#
# rf_normfinder(ct_vals)
#
#
# Normfinder2(t(ct_vals))
#
#
#
#
#
