# Returns info on files in path in a data.frame, or FALSE if the directory is empty.

FileInfo <- function(path) {
  
  path <- StripTrailingSlashes(path)
  if(!dir.exists(path)) stop("FileInfo: specified folder does not exist")
  
  if(length(dir(path))==0) {
    # folder exists and is empty
    return(FALSE)
  } else {
    file_names <- dir(path)
    info_on_files <- data.frame(directory=path,
                                file_name=file_names,
                                size_bytes=file.size(paste(path, "/", file_names, sep="")),
                                mtime=file.mtime(paste(path, "/", file_names, sep="")))
    return(info_on_files)
  }
}
