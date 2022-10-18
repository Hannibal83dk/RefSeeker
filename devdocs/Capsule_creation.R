



Rlist <- sub("./", "", list.files("./R", full.names = T))

Rlist <- Rlist[c(1,5:18, 20,21)]




detect_dependencies(Rlist)



