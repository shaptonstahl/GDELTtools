#' Download all the GDELT files to a local folder
#'
#' Downloads all (missing) GDELT files. ** This takes a long time and a lot of space. **
#' 
#' @aliases GetAllOfGDELT
#' @param local.folder character, path to the file to be validated.
#' @param historical.url.root character, URL for the folder with older GDELT files.
#' @param daily.url.root character, URL for the folder with daily GDELT files.
#' @param force logical, if TRUE then the download is carried out without further prompting the user.
#' @return logical, TRUE if all files were downloaded successfully.
#' @export
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{http://gdelt.utdallas.edu/}
#' @author 
#' \tabular{ll}{
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#' }
#' @examples
#' \dontrun{
#' GetAllOfGDELT("~/gdeltdata")} 
GetAllOfGDELT <- function(local.folder,
                          historical.url.root="http://gdelt.umn.edu/data/backfiles/",
                          daily.url.root="http://gdelt.umn.edu/data/dailyupdates/",
                          force=FALSE) {
  
  if(FALSE == force) {
    # ask the user if they are sure
    w <- strwrap(paste("The compressed GDELT data set is currently ",
                       round(GetSizeOfGDELT(), 1),
                       "GB. It will take a long time to download and requires a lot of room (",
                       round(GetSizeOfGDELT(), 1),
                       "GB) where you store it. Please verify that you have sufficient free space on the drive where you intend to store it.",
                       sep=""))
    writeLines(w)
    response <- readline("Are you ready to proceed? (y/n) ")
    
    if(FALSE == grepl("[yY]", response)) {
      return(FALSE)
    }
  }
  
  # Coerce ending slashes as needed
  StripTrailingSlashes <- function(x) {
    while( grepl("[/\\\\]$", x) ) x <- substring(x, 1, nchar(x) - 1)
    return(x)
  }
  local.folder <- StripTrailingSlashes(local.folder)
  historical.url.root <- paste(StripTrailingSlashes(historical.url.root), "/", sep="")
  daily.url.root <- paste(StripTrailingSlashes(daily.url.root), "/", sep="")
  # create the local.folder if is doesn't exist
  dir.create(local.folder, showWarnings=FALSE, recursive = TRUE)
  
  start.date <- strftime(dateParse("1979-01-01"), format="%Y-%m-%d")
  end.date <- strftime(dateShift(Sys.Date(), by="days", k.by=1, direction=-1), format="%Y-%m-%d")
  source.files <- FileListFromDates(startdate=start.date, enddate=end.date)
  source.files <- c(source.files$historic, source.files$daily)
  
  res <- sapply(source.files, function(this.file) {
    this.res <- FALSE
    
    # validate if already downloaded
    if( this.file %in% LocalVersusRemote(filelist=source.files, local.folder=local.folder)$local ) {
      if(FALSE == IsValidGDELT(f=this.file, local.folder=local.folder)) {
        # remove the offending file; it'll be downloaded in the later if/then
        file.remove(paste(local.folder, "/", this.file, sep=""))
        Sys.sleep(1)
      } else {
        this.res <- TRUE
      }
    }
    # download if not already downloaded
    if( this.file %in% LocalVersusRemote(filelist=source.files, local.folder=local.folder)$remote ) {
      download.result <- DownloadGdelt(f=this.file,
                                       local.folder=local.folder,
                                       max.local.mb=Inf,
                                       historical.url.root=historical.url.root,
                                       daily.url.root=daily.url.root,
                                       verbose=TRUE)
      if(TRUE == download.result) {
        if(FALSE == IsValidGDELT(f=this.file, local.folder=local.folder)) {
          # try again
          download.result <- DownloadGdelt(f=this.file,
                                           local.folder=local.folder,
                                           max.local.mb=Inf,
                                           historical.url.root=historical.url.root,
                                           daily.url.root=daily.url.root,
                                           verbose=TRUE)
          if(TRUE == IsValidGDELT(f=this.file, local.folder=local.folder)) {
            this.res <- TRUE
          }
        } else {
          this.res <- TRUE
        }
      }
    }
    
    # return results for this.file
    if(TRUE == this.res) {
      cat("Downloading or verifying", this.file, "succeeded.\n")
    } else {
      cat("Downloading or verifying", this.file, "FAILED.\n")
    }
    return(this.res)
  })
  
  return( all(res) )
}
