# Downloads a single file, then removes files if necessary to get under max_local_mb
# Returns TRUE if file downloaded successfully, FALSE otherwise
# DOES NOT GIVE A WARNING if non-gdelt files are in the local_folder
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.

DownloadGdelt <- function(f,
                          local_folder,
                          max_local_mb,
                          data_url_root="http://data.gdeltproject.org/events/",
                          verbose=TRUE) {
  
  # Guardians
  if(!missing(max_local_mb)) stopifnot(max_local_mb >= 0)
  # Add guardian to ensure URLs end with a slash
  # Add guardian to ensure local_folder does NOT end with a slash or backslash
  
  if(missing(local_folder)) local_folder <- tempdir()
  # Coerce ending slashes as needed
  local_folder <- StripTrailingSlashes(path.expand(local_folder))
  data_url_root <- paste(StripTrailingSlashes(data_url_root), "/", sep="")
  
  # Download the file
  op <- options()
  options(HTTPUserAgent=paste("GDELTtools v", packageVersion("GDELTtools"),
                              " in ", getOption("HTTPUserAgent"),
                              sep=""))
  result <- download.file(url=paste(data_url_root, f, sep=""),
                          destfile=paste(local_folder, "/", f, sep=""),
                          quiet=!verbose)
  if(0 != result) return(FALSE)
  options(op)
  
  # Clean up if necessary
  if(!missing(max_local_mb)) {
    info_on_files <- FileInfo(local_folder)
    mb_currently_stored <- sum(info_on_files$size, na.rm=TRUE) / 2^20

    while(mb_currently_stored > max_local_mb) {
      # delete file in folder accessed longest ago, BUT NOT CURRENT FILE
      info_on_files <- info_on_files[-which(dir(local_folder, include.dirs=FALSE)==f),]  # remove current file from consideration for deletion
      info_on_files <- info_on_files[info_on_files$size > 0,]  # remove size-zero files
      if(0 == nrow(info_on_files)) {
        # exit, because current file is the only file
        mb_currently_stored <- 0
      } else {
        old_file_ids <- which(min(info_on_files$atime, na.rm=TRUE)==info_on_files$atime)
        if(length(old_file_ids) < 1) stop("No local files to delete.")
        else del_file_id <- old_file_ids[1]
        
        file.remove(paste(local_folder, "/", dir(local_folder, include.dirs=FALSE)[del_file_id], sep=""))
        
        # update
        info_on_files <- FileInfo(local_folder)
        mb_currently_stored <- sum(info_on_files$size, na.rm=TRUE) / 2^20
      }
    }
  }
  return(TRUE)
}
