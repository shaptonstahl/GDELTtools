# Download a list of all of the individual files in the dataset.
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.
#
# ex: v1_event_files <- ListAllGDELTFiles(version=1, data_type="events", local_folder="~/gdeltdata")
#
# ex: v2_gkg_files <- ListAllGDELTFiles(version=2, data_type="gkg", local_folder="~/gdeltdata")

ListAllGDELTFiles <- function(version, 
                              data_type=c("events","gkg","gkgcounts","mentions"),
                              local_folder,
                              timeout=300) {
  
  if(missing(version) || missing(local_folder)) {
    stop("ListAllGDELTFiles: must specify version and local_folder")
  }
  
  metadata <- DataFileMetadata(version=version, local_folder=local_folder, 
                               data_type=data_type, timeout=timeout)
  return(metadata$file_name)
}
