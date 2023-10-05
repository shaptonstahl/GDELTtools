# Check to see if a file is local. If not, Download the file.
# There are no checks to ensure that the file is not taking up
# too much space locally. Does not check the MD5 hash to ensure
# the file is not corrupted. If the file is a metadata file, it 
# checks to see if it's more than a day old; if so, it downloads
# the file again.

DownloadIfMissing <- function(file_name, 
                              url, 
                              local_folder, 
                              verbose=TRUE, 
                              timeout=300) {
  
  is_remote <- file_name %in% LocalVersusRemote(filelist=file_name, 
                                                local_folder=local_folder)$remote
  if(is_remote || (!is_remote & 
                   substr(file_name, 1, 1)=="v" &
                   file.info(paste(local_folder, "/", file_name, sep=""))$atime < Sys.time() - 1)) {
    op <- options()
    options(HTTPUserAgent=paste("GDELTtools v", packageVersion("GDELTtools"),
                                " in ", getOption("HTTPUserAgent"),
                                sep=""))
    options(timeout=timeout)
    result <- download.file(url=url,
                            destfile=paste(local_folder, "/", file_name, sep=""),
                            quiet=!verbose,)
    if(0 != result) stop(paste("DataFileMetadata: error downloading", file_name))
    options(op)
    return(result==0)
  } else {
    # File already exists locally
    return(TRUE)
  }
}
