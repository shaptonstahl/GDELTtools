DownloadGdelt <- function(f,
                          local.folder,
                          max.local.mb,
                          historical.url.root="http://gdelt.umn.edu/data/backfiles/",
                          daily.url.root="http://gdelt.umn.edu/data/dailyupdates/",
                          verbose=TRUE) {
  # Dowloads a single file, then removes files if necessary to get under max.local.mb
  # Returns TRUE if file downloaded successfully, FALSE otherwise
  # DOES NOT GIVE A WARNING if non-gdelt files are in the local.folder
  
  # Guardians
  if(!missing(max.local.mb)) stopifnot(max.local.mb >= 0)
  # Add guardian to ensure URLs end with a slash
  # Add guardian to ensure local.folder does NOT end with a slash or backslash
  
  # Constants
  historical.names.zips <- c("201303.zip", "201302.zip", "201301.zip", "201212.zip", "201211.zip", "201210.zip", "201209.zip", "201208.zip", "201207.zip", "201206.zip", "201205.zip", "201204.zip", "201203.zip", "201202.zip", "201201.zip", "201112.zip", "201111.zip", "201110.zip", "201109.zip", "201108.zip", "201107.zip", "201106.zip", "201105.zip", "201104.zip", "201103.zip", "201102.zip", "201101.zip", "201012.zip", "201011.zip", "201010.zip", "201009.zip", "201008.zip", "201007.zip", "201006.zip", "201005.zip", "201004.zip", "201003.zip", "201002.zip", "201001.zip", "200912.zip", "200911.zip", "200910.zip", "200909.zip", "200908.zip", "200907.zip", "200906.zip", "200905.zip", "200904.zip", "200903.zip", "200902.zip", "200901.zip", "200812.zip", "200811.zip", "200810.zip", "200809.zip", "200808.zip", "200807.zip", "200806.zip", "200805.zip", "200804.zip", "200803.zip", "200802.zip", "200801.zip", "200712.zip", "200711.zip", "200710.zip", "200709.zip", "200708.zip", "200707.zip", "200706.zip", "200705.zip", "200704.zip", "200703.zip", "200702.zip", "200701.zip", "200612.zip", "200611.zip", "200610.zip", "200609.zip", "200608.zip", "200607.zip", "200606.zip", "200605.zip", "200604.zip", "200603.zip", "200602.zip", "200601.zip", "2005.zip", "2004.zip", "2003.zip", "2002.zip", "2001.zip", "2000.zip", "1999.zip", "1998.zip", "1997.zip", "1996.zip", "1995.zip", "1994.zip", "1993.zip", "1992.zip", "1991.zip", "1990.zip", "1989.zip", "1988.zip", "1987.zip", "1986.zip", "1985.zip", "1984.zip", "1983.zip", "1982.zip", "1981.zip", "1980.zip", "1979.zip")
  if(f %in% historical.names.zips) {
    file.is.historical <- TRUE
    url.root <- historical.url.root
  } else {
    file.is.historical <- FALSE
    url.root <- daily.url.root
  }
  if(missing(local.folder)) local.folder <- tempdir()
  
  # Download the file
  op <- options()
  options(HTTPUserAgent=paste("GDELTtools v", packageVersion("GDELTtools"),
                              " in ", getOption("HTTPUserAgent"),
                              sep=""))
  result <- download.file(url=paste(url.root, f, sep=""),
                          destfile=paste(local.folder, "/", f, sep=""),
                          quiet=!verbose)
  if(0 != result) return(FALSE)
  options(op)
  
  # Clean up if necessary
  if(!missing(max.local.mb)) {
    info.on.files <- FileInfo(local.folder)
    mb.currently.stored <- sum(info.on.files$size, na.rm=TRUE) / 2^20
    #browser()
    while(mb.currently.stored > max.local.mb) {
      # delete file in folder accessed longest ago, BUT NOT CURRENT FILE
      info.on.files <- info.on.files[-which(dir(local.folder, include.dirs=FALSE)==f),]  # remove current file from consideration for deletion
      info.on.files <- info.on.files[info.on.files$size > 0,]  # remove size-zero files
      if(0 == nrow(info.on.files)) {
        # exit, because current file is the only file
        mb.currently.stored <- 0
      } else {
        old.file.ids <- which(min(info.on.files$atime, na.rm=TRUE)==info.on.files$atime)
        if(length(old.file.ids) < 1) stop("No local files to delete.")
        else del.file.id <- old.file.ids[1]
        
        file.remove(paste(local.folder, "/", dir(local.folder, include.dirs=FALSE)[del.file.id], sep=""))
        
        # update
        info.on.files <- FileInfo(local.folder)
        mb.currently.stored <- sum(info.on.files$size, na.rm=TRUE) / 2^20
      }
    }
  }
  return(TRUE)
}
