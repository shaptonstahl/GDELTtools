# Downloads a single event data file, then removes files if necessary to get 
# under max_local_mb. Returns TRUE if file downloaded successfully, FALSE 
# otherwise. DOES NOT GIVE A WARNING if non-gdelt files are in the local_folder
# Note: Using http instead of https because the certificate is expired
# on the GDELT site.

DownloadGdelt <- function(f,
                          local_folder,
                          max_local_mb,
                          version,
                          data_type=c("events","gkg","gkgcounts","mentions"),
                          verbose=TRUE,
                          timeout=300) {
  
  local_folder <- StripTrailingSlashes(path.expand(local_folder))
  data_url_root <- DataURLRoot(version=version, data_type=data_type)
  
  # Download the file
  download_result <- DownloadIfMissing(file_name=f, 
                                       url=paste(data_url_root, f, sep=""), 
                                       local_folder=local_folder,
                                       verbose=verbose,
                                       timeout=timeout)
  if(!download_result) return(download_result) # no cleanup if file not present
  
  # Clean up if necessary
  enforcement_result <- EnforceMaxDownloads(max_local_mb=max_local_mb,
                                            local_folder=local_folder,
                                            files_to_protect=f)
  if(!enforcement_result) warning("DownloadGDELT: Unable to stay under max_local_mb")  
  return(download_result)
}
