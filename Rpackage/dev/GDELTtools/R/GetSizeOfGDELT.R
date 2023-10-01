# Returns the size of the complete data set, compressed, in GB.
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.
#
# ex: GetSizeOfGDELT(version=1, data_type="events", local_folder="~/gdeltdata")
# ex: GetSizeOfGDELT(version=1, data_type="gkg", local_folder="~/gdeltdata")
# ex: GetSizeOfGDELT(version=1, data_type="gkgcounts", local_folder="~/gdeltdata")
# ex: GetSizeOfGDELT(version=2, data_type="events", local_folder="~/gdeltdata")
# ex: GetSizeOfGDELT(version=2, data_type="gkg", local_folder="~/gdeltdata")
# ex: GetSizeOfGDELT(version=2, data_type="mentions", local_folder="~/gdeltdata")

GetSizeOfGDELT <- function(version,
                           data_type=c("events","gkg","gkgcounts","mentions"),
                           local_folder,
                           timeout=300) {
  
  if(missing(version)) stop("GetSizeOfGDELT: must specify version as 1 or 2")
  if(missing(local_folder)) stop("GetSizeOfGDELT: must specify a local_folder for saving the metadata file")
  
  metadata <- DataFileMetadata(version=version,
                               data_type=data_type,
                               local_folder=local_folder,
                               timeout=timeout)
  
  gb <- sum(metadata$size_bytes) / (1024^3)
  names(gb) <- "GB"
  return(gb)
}
