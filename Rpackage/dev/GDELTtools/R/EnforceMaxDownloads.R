# While more than max_local_mb is stored in local_folder, remove the oldest 
# (based on access time) files. Returns TRUE if able to get below max_local_mb.

EnforceMaxDownloads <- function(max_local_mb,
                                local_folder,
                                files_to_protect) {
  
  local_folder <- StripTrailingSlashes(local_folder)
  info_on_files <- FileInfo(local_folder)
  mb_currently_stored <- sum(info_on_files$size_bytes, na.rm=TRUE) / 2^20
  
  while(mb_currently_stored > max_local_mb) {
    ####  delete file in folder modified longest ago  ####
    
    # remove files_to_protect from consideration for deletion
    info_on_files <- info_on_files[!(info_on_files$file_name %in% files_to_protect),]  
    # remove size-zero files
    info_on_files <- info_on_files[info_on_files$size > 0,]  
    
    if(0 == nrow(info_on_files)) {
      # exit, because files_to_protect are the only files
      return(FALSE)
    } else {
      old_file <- info_on_files$file_name[which.min(info_on_files$mtime)]
      if(length(old_file) < 1) return(FALSE)

      file.remove(paste(local_folder, "/", old_file, sep=""))
      
      # update
      info_on_files <- FileInfo(local_folder)
      mb_currently_stored <- sum(info_on_files$size_bytes, na.rm=TRUE) / 2^20
    }
  }
  return(TRUE)
}