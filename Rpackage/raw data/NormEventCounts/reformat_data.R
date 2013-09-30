# setwd() to folder containing raw data

NormEventCounts.files <- dir(pattern=".RData")

temp.env <- new.env()
load(NormEventCounts.files[1], envir=temp.env)
NormEventCountsData <- list(eval(as.name(ls(envir=temp.env)[1]), envir=temp.env))
names(NormEventCountsData) <- ls(envir=temp.env)[1]
rm(list=ls(envir=temp.env)[1], envir=temp.env)
while(length(ls(envir=temp.env)) > 0) {
  NormEventCountsData <- c(NormEventCountsData, list(eval(as.name(ls(envir=temp.env)[1]), envir=temp.env)))
  names(NormEventCountsData)[length(NormEventCountsData)] <- ls(envir=temp.env)[1]
  rm(list=ls(envir=temp.env)[1], envir=new.env)
}

for(i in 2:length(NormEventCounts.files)) {
  load(NormEventCounts.files[i], envir=temp.env)
  while(length(ls(envir=temp.env)) > 0) {
    NormEventCountsData <- c(NormEventCountsData, list(eval(as.name(ls(envir=temp.env)[1]), envir=temp.env)))
    names(NormEventCountsData)[length(NormEventCountsData)] <- ls(envir=temp.env)[1]
    rm(list=ls(envir=temp.env)[1], envir=temp.env)
  }
}

# names(NormEventCountsData)

save(NormEventCountsData, file="NormEventCountsData.RData", compress="bzip2")
