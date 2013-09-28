GetGdelt <- function(start.date,
                     end.date=start.date,
                     filter,
                     local.folder,
                     max.local.mb,
                     allow.wildcards=FALSE, 
                     use.regex=FALSE,
                     historical.url.root="http://gdelt.utdallas.edu/data/backfiles/",
                     daily.url.root="http://gdelt.utdallas.edu/data/dailyupdates/",
                     verbose=FALSE) {
  
  out.initialized <- FALSE
  
  # Determine file list based on dates
  source.files <- FileListFromDates(startdate=start.date, enddate=end.date)
  source.files <- c(source.files$historic, source.files$daily)
  
  # Ingest and filter local files
  for(this.file in LocalVersusRemote(filelist=source.files, local.folder=local.folder)$local) {
    new.data <- GdeltZipToDataframe(f=paste(local.folder, "/", this.file, sep=""))
    
    browser()
    
    new.data <- FilterGdeltDataframe(x=new.data,
                                     filter=filter,
                                     allow.wildcards=allow.wildcards,
                                     use.regex=use.regex)
    if(out.initialized) {
      out <- rbind(out, new.data)
    } else {
      out <- new.data
      out.initialized <- TRUE
    }
  }
  
  # Download, ingest, and filter remote files
  for(this.file in LocalVersusRemote(filelist=source.files, local.folder=local.folder)$remote) {
    download.result <- DownloadGdelt(f=this.file,
                                     local.folder=local.folder,
                                     max.local.mb=max.local.mb,
                                     historical.url.root=historical.url.root,
                                     daily.url.root=daily.url.root,
                                     verbose=FALSE)
    new.data <- GdeltZipToDataframe(f=paste(local.folder, "/", this.file, sep=""))
    new.data <- FilterGdeltDataframe(x=new.data,
                                     filter=filter,
                                     allow.wildcards=allow.wildcards,
                                     use.regex=use.regex)
    if(out.initialized) {
      out <- rbind(out, new.data)
    } else {
      out <- new.data
      out.initialized <- TRUE
    }
  }
  
  # Filter one more time on dates
  
  return(out)
}
