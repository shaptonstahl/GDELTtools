# Verify the integrity of a GDELT data file
#
# Compares the MD5 hash of a downloaded file to the known hash provided
# on the server.
# 
IsValidGDELT <- function(f,
                         local_folder) {
  
  md5_url <- "http://data.gdeltproject.org/events/md5sums"
  
  md5_df <- tryCatch(read.delim(md5_url, sep=" ", header=FALSE, stringsAsFactors=FALSE), 
                     error=function(e) stop(simpleError(paste("unable to read MD5 file at", md5_url), 
                                                        "IsValidGDELT")))
  
  this_md5 <- md5_df[ md5_df[,ncol(md5_df)]==f ,1]
  if(length(this_md5) != 1) {
    warning("Unable to find MD5 for ", f)
    return(FALSE)
  }
  
  observed_md5 <- tryCatch(md5sum(paste(StripTrailingSlashes(local_folder), "/", f, sep="")),
                           error=function(e) stop(simpleError("unable to calculate MD5 for downloaded file",
                                                              "IsValidGDELT")))
  
  return( observed_md5 == this_md5 )
}
