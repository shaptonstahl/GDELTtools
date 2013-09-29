#' Download and subset GDELT data
#'
#' Download the GDELT files necessary for a data set, import them,
#' filter on various crieteria, and return a data.frame. 
#' 
#' @aliases GetGDELT
#' @param start.date character, just about any human-readable form of the earliest date to include.
#' @param end.date character, just about any human-readable form of the latest date to include.
#' @param filter list, named list encoding the values to include for specified fields. See Details.
#' @param local.folder character, if specified, where downloaded files will be saved.
#' @param max.local.mb numeric, the maximum size in MB of the downloaded files that will be retained.
#' @param allow.wildcards logical, must be TRUE to use * in \code{filter} to specify 'any character(s)'.
#' @param use.regex logical, if TRUE then \code{filter} will be processed as a \code{\link{regular expression}}.
#' @param historical.url.root character, URL from which historical files will be downloaded.
#' @param daily.url.root character, URL from which daily files will be downloaded.
#' @return verbose logical, if TRUE then indications of progress will be displayed.
#' @export
#' @details
#' 
#' These are some details.
#' 
#' @section Filtering Results:
#' 
#' This is how you write the \code{filter}.
#' 
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{http://gdelt.utdallas.edu/}
#' @author 
#' \tabular{ll}{
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#'   Thomas Scherer \tab \email{tscherer@@princeton.edu}\cr
#'   John Beieler \tab \email{jub270@@psu.edu}\cr
#' }
#' @examples
#' data(iris)   # provides example data
#' x <- Function(1, 2)     # Does something simple
#' x                       # Display result
#' 
#' y <- Function(1, 2, 3)  # Does something more complicated
#' y                       # Display result
GetGDELT <- function(start.date,
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
