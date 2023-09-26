# Given a list of files, determines which ones are local and which are remote

LocalVersusRemote <- function(filelist, local_folder) {
  
  return( list(local=filelist[filelist %in% dir(local_folder)],
               remote=filelist[!(filelist %in% dir(local_folder))]) )
}
