# Returns info on files in path in a data.frame

FileInfo <- function(path, include_dirs=FALSE) {
  info_on_files <- ldply(paste(path, "/", dir(path), sep=""), file.info)
  info_on_files <- data.frame(name=dir(path), info_on_files, stringsAsFactors=FALSE)
  if(!include_dirs) info_on_files <- info_on_files[!info_on_files$isdir,]
  return(info_on_files)
}
