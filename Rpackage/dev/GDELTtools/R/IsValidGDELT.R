# Verify the integrity of a GDELT data file
#
# Compares the MD5 hash of a downloaded file to the known hash provided
# on the server.
# 
#
# ex: IsValidGDELT("1979.zip", local_folder="~/gdeltdata", version=1, data_type="events")
# ex: IsValidGDELT("20150218230000.export.CSV.zip", local_folder="~/gdeltdata", version=2, data_type="events")

IsValidGDELT <- function(x,
                         local_folder,
                         version,
                         data_type=c("events","gkg","gkgcounts","mentions"),
                         timeout=300) {
  
  metadata <- DataFileMetadata(version=version, local_folder=local_folder, 
                               data_type=data_type, timeout=timeout)
  this_md5 <- metadata$md5[metadata$file_name==x]
  if(length(this_md5) != 1) {
    warning("Unable to find MD5 for ", x)
    return(FALSE)
  }
  
  observed_md5 <- tryCatch(md5sum(paste(StripTrailingSlashes(local_folder), "/", x, sep="")),
                           error=function(e) stop(simpleError("unable to calculate MD5 for downloaded file",
                                                              "IsValidGDELT")))
  return( observed_md5 == this_md5 )
}
