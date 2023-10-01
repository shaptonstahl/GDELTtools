# Given the version and type of data returns the URL of the folder where the
# data files are stored on the GDELT site. Includes a trailing slash.

DataURLRoot <- function(version, 
                        data_type=c("events","gkg","gkgcounts","mentions")) {
  if(1==version) {
    if("events"==data_type) {
      return("http://data.gdeltproject.org/events/")
    } else if("gkg"==data_type | "gkgcounts"==data_type) {
      return("http://data.gdeltproject.org/gkg/")
    } else if("mentions"==data_type) {
      stop("DataURLRoot: mentions data not available for V1")
    } else {
      stop("DataURLRoot: For version 1 data_type must be one of events, gkg, or gkgcounts")
    }
  } else if(2==version) {
    if("events"==data_type | "gkg"==data_type | "mentions"==data_type) {
      return("http://data.gdeltproject.org/gdeltv2/")
    } else if("gkgcounts"==data_type) {
      stop("DataURLRoot: gkgcounts data not available for V2")
    } else {
      stop("DataURLRoot: For version 2 data_type must be one of events, gkg, or mentions")
    }
  }
}