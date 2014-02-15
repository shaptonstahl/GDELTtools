ListAllGDELTFiles <- function(as.of=Sys.Date() - 1) {
  compressed.files <- c(paste(1979:2005, ".masterevents.zip", sep=""),
                        paste(format(seq(from=as.Date("2006-01-01"), to=as.Date("2012-08-01"), by="month"), "%Y%m"), ".masterevents.zip", sep=""),
                        paste(format(seq(from=as.Date("2012-09-01"), to=as.Date("2013-03-01"), by="day"), "%Y%m%d"), ".masterevents.zip", sep=""),
                        paste(format(seq(from=as.Date("2012-09-01"), to=as.Date(as.of), by="day"), "%Y%m%d"), ".export.CSV.zip", sep=""))

}