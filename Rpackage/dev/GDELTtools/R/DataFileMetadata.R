# Get metadata (name, size, md5) for data files
# 
# Returns a tibble with columns: file_name, size_bytes, md5
# 
# ex: v1_events    <- DataFileMetadata(version=1, data_type="events", local_folder="~/gdeltdata")
# ex: v1_gkg       <- DataFileMetadata(version=1, data_type="gkg", local_folder="~/gdeltdata")
# ex: v1_gkgcounts <- DataFileMetadata(version=1, data_type="gkgcounts", local_folder="~/gdeltdata")
# ex: v2_events    <- DataFileMetadata(version=2, data_type="events", local_folder="~/gdeltdata")
# ex: v2_gkg       <- DataFileMetadata(version=2, data_type="gkg", local_folder="~/gdeltdata")
# ex: v2_mentions  <- DataFileMetadata(version=2, data_type="mentions", local_folder="~/gdeltdata")

DataFileMetadata <- function(version,
                             data_type=c("events","gkg","gkgcounts","mentions"),
                             local_folder,
                             timeout=300) {
  if(missing(version)) stop("DataFileMetadata: must specify version")
  if(version==2 & missing(local_folder)) stop("ListAllGDELTFiles: for V2 must specify local_folder")
  
  if(1==version) {
    DownloadIfMissing(file_name="v1_event_file_list.txt", 
                      url="http://data.gdeltproject.org/events/filesizes",
                      local_folder=local_folder, timeout=timeout)
    DownloadIfMissing(file_name="v1_event_md5sums.txt", 
                      url="http://data.gdeltproject.org/events/md5sums",
                      local_folder=local_folder, timeout=timeout)
    DownloadIfMissing(file_name="v1_gkg_file_list.txt", 
                      url="http://data.gdeltproject.org/gkg/filesizes",
                      local_folder=local_folder, timeout=timeout)
    DownloadIfMissing(file_name="v1_gkg_md5sums.txt", 
                      url="http://data.gdeltproject.org/gkg/md5sums",
                      local_folder=local_folder, timeout=timeout)
    if("events"==data_type) {
      suppressWarnings(
        suppressMessages(
          events_df <- read_delim(file=paste(local_folder, "/v1_event_file_list.txt", sep=""),
                                  skip=1, delim=" ", col_names=c("size_bytes","file_name"))))
      
      suppressWarnings(
        suppressMessages(
          md5_df <- read_delim(file=paste(local_folder, "/v1_event_md5sums.txt", sep=""),
                               skip=1, delim=" ", col_select=c(1,3),
                               col_names=c("md5", "x2", "file_name"))))
    } else if("gkg"==data_type | "gkgcounts"==data_type) {
      suppressWarnings(
        suppressMessages(
          events_df <- read_delim(file=paste(local_folder, "/v1_gkg_file_list.txt", sep=""),
                                  delim=" ", col_names=c("size_bytes","file_name"))))
      
      suppressWarnings(
        suppressMessages(
          md5_df <- read_delim(file=paste(local_folder, "/v1_gkg_md5sums.txt", sep=""),
                               delim=" ", col_select=c(1,3),
                               col_names=c("md5", "x2", "file_name"))))

    } else if("mentions"==data_type) {
      stop("DataFileMetadata: there is no mentions data for version 1")
    } else {
      stop("DataFileMetadata: data_type for v1 must be events, gkg, or gkgcounts")
    }
    
    events_df <- events_df[!duplicated(events_df),]
    md5_df <- md5_df[!duplicated(md5_df),]
    merged_df <- full_join(events_df, md5_df, by=join_by("file_name"))[,c("file_name","size_bytes","md5")]
    
    if("gkg"==data_type) {
      merged_df <- merged_df[grepl("gkg.csv.zip", merged_df$file_name, fixed=TRUE),]
    } else if("gkgcounts"==data_type) {
      merged_df <- merged_df[grepl("gkgcounts", merged_df$file_name, fixed=TRUE),]
    } else {
      # events; do nothing
    }
    
    return(merged_df)
  } else if(2==version) {
    DownloadIfMissing(file_name="v2_masterfilelist_english.txt", 
                      url="http://data.gdeltproject.org/gdeltv2/masterfilelist.txt",
                      local_folder=local_folder, timeout=timeout)
#    DownloadIfMissing(file_name="v2_masterfilelist_translingual.txt", 
#                      url="http://data.gdeltproject.org/gdeltv2/masterfilelist-translation.txt",
#                      local_folder=local_folder, timeout=timeout)
    suppressWarnings(
      suppressMessages(
        merged_df <- read_delim(paste(local_folder, "/v2_masterfilelist_english.txt", sep=""),
                                delim=" ", col_names = c("size_bytes","md5","file_name"),
                                progress=FALSE)[,c("file_name","size_bytes","md5"),]))
    
    merged_df$file_name <- sapply(strsplit(merged_df$file_name, split="/"), function(x) x[5])
    
    if("events"==data_type) {
      merged_df <- merged_df[grepl("export", merged_df$file_name, fixed=TRUE),]
    } else if("gkg"==data_type) {
      merged_df <- merged_df[grepl("gkg", merged_df$file_name, fixed=TRUE),]
    } else if("gkgcounts"==data_type) {
      stop("DataFileMetadata: there is no gkgcounts data for version 2")
    } else if("mentions"==data_type) {
      merged_df <- merged_df[grepl("mentions", merged_df$file_name, fixed=TRUE),]
    } else {
      stop("DataFileMetadata: data_type for v2 must be events, gkg, or mentions")
    }
    return(merged_df)
  } else {
    stop("DataFileMetadata: version must be 1 or 2")
  }
}