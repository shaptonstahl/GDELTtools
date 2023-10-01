# Returns the size of the complete data set, compressed, in GB.
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.
#
# ex: GetSizeOfGDELT(version=1, local_folder="~/gdeltdata", data_type="events")
# ex: GetSizeOfGDELT(version=1, local_folder="~/gdeltdata", data_type="gkg")
# ex: GetSizeOfGDELT(version=1, local_folder="~/gdeltdata", data_type="gkgcounts")
# ex: GetSizeOfGDELT(version=2, local_folder="~/gdeltdata", data_type="events")
# ex: GetSizeOfGDELT(version=2, local_folder="~/gdeltdata", data_type="gkg")
# ex: GetSizeOfGDELT(version=2, local_folder="~/gdeltdata", data_type="mentions")

GetSizeOfGDELT <- function(version,
                           local_folder,
                           data_type=c("events","gkg","gkgcounts","mentions"),
                           timeout=300) {
  
  if(missing(version)) stop("GetSizeOfGDELT: must specify version as 1 or 2")
  if(missing(local_folder)) stop("GetSizeOfGDELT: must specify a local_folder for saving the metadata file")
  
  metadata <- DataFileMetadata(version=version,
                               local_folder=local_folder,
                               data_type=data_type,
                               timeout=timeout)
  
  gb <- sum(metadata$size_bytes) / (1024^3)
  names(gb) <- "GB"
  return(gb)
}
