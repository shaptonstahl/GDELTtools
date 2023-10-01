# Download a list of all of the individual files in the dataset.
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.
#
# ex: v1_event_files <- ListAllGDELTFiles(version=1, local_folder="~/gdeltdata", data_type="events")
#
# ex: v2_gkg_files <- ListAllGDELTFiles(version=2, local_folder="~/gdeltdata", data_type="gkg")

ListAllGDELTFiles <- function(version, 
                              local_folder,
                              data_type=c("events","gkg","gkgcounts","mentions"),
                              timeout=300) {
  
  if(missing(version) || missing(local_folder)) {
    stop("ListAllGDELTFiles: must specify version and local_folder")
  }
  
  metadata <- DataFileMetadata(version=version, local_folder=local_folder, 
                               data_type=data_type, timeout=timeout)
  return(metadata$file_name)
}
