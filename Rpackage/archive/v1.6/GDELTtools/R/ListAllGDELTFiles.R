# Download a list of all of the individual files in the dataset.
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.

ListAllGDELTFiles <- function(filesize_url="http://data.gdeltproject.org/events/filesizes") {
  fs <- read.delim(filesize_url, header=FALSE, sep=" ")
  names(fs) <- c("size_bytes", "file_name")
  return( fs$file_name[2:nrow(fs)] )
}
