GetSizeOfGDELT <- function(historical.url.root="http://gdelt.umn.edu/data/backfiles/",
                           daily.url.root="http://gdelt.umn.edu/data/dailyupdates/") {
  
  # Returns the size of the complete data set, compressed, in GB
  
  historical.raw <- readLines(historical.url.root)
  hs <- historical.raw[grep("[0-9\\.]+M *$", historical.raw)]
  hs <- gsub("<a.+</a> *", "", hs)
  hs <- gsub("M.*$", "", hs)
  hs.mb <- sum(as.numeric(hs))
  hs.gb <- hs.mb / 1024
  
  daily.raw <- readLines(daily.url.root)
  ds <- daily.raw[grep("[0-9\\.]+M *$", daily.raw)]
  ds <- gsub("<a.+</a> *", "", ds)
  ds <- gsub("M.*$", "", ds)
  ds.mb <- sum(as.numeric(ds))
  ds.gb <- ds.mb / 1024
  
  return(hs.gb + ds.gb)
}