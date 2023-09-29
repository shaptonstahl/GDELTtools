# Returns the size of the complete data set, compressed, in GB.
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.

GetSizeOfGDELT <- function(filesize_url="http://data.gdeltproject.org/events/filesizes") {
  
  fs <- read.delim(filesize_url, header=FALSE, sep=" ")
  names(fs) <- c("size_bytes", "file_name")
  fs$size_bytes <- as.numeric(fs$size_bytes)
  fs <- fs[fs$file_name %in% ListAllGDELTFiles(),]
  gb <- sum(fs$size_bytes) / (1024^3)
  names(gb) <- "GB"
  return(gb)
}
