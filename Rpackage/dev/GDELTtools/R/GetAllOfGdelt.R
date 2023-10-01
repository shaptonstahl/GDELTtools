#' Download all the GDELT V1 Event files to a local folder
#'
#' Downloads all GDELT V1 Event files not already present locally. ** This takes a long time and a lot of space. **
#' 
#' @aliases GetAllOfGDELT
#' @param local_folder character, path to the file to be validated.
#' @param version numeric, specifying version 1 or 2 of the GDELT data.
#' @param data_type character, One of events, gkg, gkgcounts, or mentions.
#' @param verbose logical, if TRUE then messages will be displayed during the downloads.
#' @param timeout integer, maximum number of seconds allowed for downloading each file.
#' @param force logical, if TRUE then the download is carried out without further prompting the user.
#' @return logical, TRUE if all files were downloaded successfully.
#' @export
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2013.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{https://www.gdeltproject.org/}
#' @author 
#' \tabular{ll}{
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#' }
#' @examples
#' \dontrun{
#' GetAllOfGDELT("~/gdeltdata")} 
GetAllOfGDELT <- function(local_folder,
                          version,
                          data_type=c("events","gkg","gkgcounts","mentions"),
                          verbose=TRUE,
                          timeout=300,
                          force=FALSE) {
  
  if(FALSE == force) {
    # ask the user if they are sure
    size_of_GDELT <- GetSizeOfGDELT(version=version, 
                                    local_folder=local_folder, 
                                    data_type=data_type,
                                    timeout=timeout)
    w <- strwrap(paste("The requested GDELT data set is currently ",
                       round(size_of_GDELT, 1),
                       "GB in compressed files. It will take a long time to download and requires a lot of room (",
                       round(size_of_GDELT, 1),
                       "GB) where you store it. Please verify that you have sufficient free space on the drive where you intend to store it.",
                       sep=""))
    writeLines(w)
    response <- readline("Are you ready to proceed? (y/n) ")
    
    if(FALSE == grepl("[yY]", response)) {
      return(FALSE)
    }
  }
  
  # Coerce ending slashes as needed
  local_folder <- StripTrailingSlashes(path.expand(local_folder))
  data_url_root <- DataURLRoot(version=version, data_type=data_type)
  # create the local_folder if is doesn't exist
  dir.create(local_folder, showWarnings=FALSE, recursive = TRUE)
  
  source_files <- ListAllGDELTFiles(version=version, 
                                    data_type=data_type,
                                    local_folder=local_folder, 
                                    timeout=timeout)
  
  res <- sapply(source_files, function(this_file) {
    this_res <- FALSE
    
    # validate if already downloaded
    if( this_file %in% LocalVersusRemote(filelist=source_files, local_folder=local_folder)$local ) {
      if(FALSE == IsValidGDELT(x=this_file, local_folder=local_folder,
                               version=version, data_type=data_type,
                               timeout=timeout)) {
        # remove the offending file; it'll be downloaded in the later if/then
        file.remove(paste(local_folder, "/", this_file, sep=""))
        Sys.sleep(1)
      } else {
        this_res <- TRUE
      }
    }
    # download if not already downloaded
    if( this_file %in% LocalVersusRemote(filelist=source_files, local_folder=local_folder)$remote ) {
      download_result <- DownloadGdelt(f=this_file,
                                       local_folder=local_folder,
                                       max_local_mb=Inf,
                                       version=version,
                                       data_type=data_type,
                                       verbose=TRUE,
                                       timeout=timeout)
      if(TRUE == download_result) {
        if(FALSE == IsValidGDELT(x=this_file, local_folder=local_folder,
                                 version=version, data_type=data_type,
                                 timeout=timeout)) {
          # try again
          download_result <- DownloadGdelt(f=this_file,
                                           local_folder=local_folder,
                                           max_local_mb=Inf,
                                           version=version,
                                           data_type=data_type,
                                           verbose=TRUE,
                                           timeout=timeout)
          if(TRUE == IsValidGDELT(x=this_file, local_folder=local_folder,
                                  version=version, data_type=data_type,
                                  timeout=timeout)) {
            this_res <- TRUE
          }
        } else {
          this_res <- TRUE
        }
      }
    }
    
    # return results for this_file
    if(TRUE == this_res) {
      cat("Downloading or verifying", this_file, "succeeded.\n\n")
    } else {
      cat("Downloading or verifying", this_file, "FAILED.\n\n")
    }
    return(this_res)
  })
  
  return( all(res) )
}
