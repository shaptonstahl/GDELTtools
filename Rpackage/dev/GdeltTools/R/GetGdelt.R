#' Download and subset GDELT data
#'
#' Download the GDELT files necessary for a data set, import them, filter on various criteria, and return a data.frame. 
#' 
#' @aliases GetGDELT
#' @param start_date character, just about any human-readable form of the earliest date to include.
#' @param end_date character, just about any human-readable form of the latest date to include.
#' @param local_folder character, if specified, where downloaded files will be saved.
#' @param max_local_mb numeric, the maximum size in MB of the downloaded files that will be retained.
#' @param data_url_root character, URL for the folder with GDELT data files.
#' @param verbose logical, if TRUE then indications of progress will be displayed_
#' @param row_filter <data-masking> Row selection. Expressions that return a logical value, and are defined in terms of the variables in GDELT. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#' @param ... <tidy-select>, Column selection. This takes the form of one or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.
#' @return data.frame
#' @export
#' @details
#' 
#' Dates are parsed with \code{dateParse} in the TimeWarp package. 
#' Years must be given with four digits.
#'
#' If \code{local_folder} is not specified then downloaded files are stored in
#' \code{tempdir()}. If a needed file has already been downloaded to \code{local_folder}
#' then this file is used instead of being downloaded. This can greatly speed up future
#' downloads.
#' 
#' @section Filtering Results:
#' 
#' The \code{row_filter} is passed to \code{\link[dplyr]{filter}}. This is a very flexible way to filter
#' the rows. It's well worth checking out the \code{\link[dplyr]{filter}} documentation.
#' 
#' @section Selecting Columns:
#' 
#' The \code{...} is passed to \code{\link[dplyr]{select}}. This is a very flexible way to choose
#' which columns to return. It's well worth checking out the \code{\link[dplyr]{select}} documentation.
#' 
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2013.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{https://www.gdeltproject.org/}
#' @author 
#' \tabular{ll}{
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#'   Thomas Scherer \tab \email{tscherer@@princeton.edu}\cr
#'   John Beieler \tab \email{jub270@@psu.edu}\cr
#' }
#' @examples
#' \dontrun{
#' test_results <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
#'   row_filter=ActionGeo_CountryCode="US")
#' 
#' test_filter <- list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US")
#' test_results <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31", 
#'   filter=test_filter)
#' table(test_results$ActionGeo_ADM1Code)
#' table(test_results$ActionGeo_CountryCode)}
#' 
#' # Specify a local folder to store the downloaded files
#' \dontrun{
#' test_results <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31", 
#'                          filter=test_filter,
#'                          local_folder="~/gdeltdata",
#'                          max_local_mb=500)}
GetGDELT <- function(start_date,
                     end_date=start_date,
                     local_folder=tempdir(),
                     max_local_mb=Inf,
                     data_url_root="http://data.gdeltproject.org/events/",
                     verbose=TRUE,
                     row_filter=TRUE,
                     ...) {
  
  # Coerce ending slashes as needed
  local_folder <- StripTrailingSlashes(path.expand(local_folder))
  data_url_root <- paste(StripTrailingSlashes(data_url_root), "/", sep="")
  # create the local_folder if is doesn't exist
  dir.create(local_folder, showWarnings=FALSE, recursive = TRUE)
  
  start_date <- strftime(dateParse(start_date), format="%Y-%m-%d")
  end_date <- strftime(dateParse(end_date), format="%Y-%m-%d")
  
  if(start_date <= "2014-01-25" & end_date >= "2014-01-23") warning("Your date range includes some or all of Feb 23-25, 2014. GDELT data is not available for these dates.")
  
  out_initialized <- FALSE
  
  # Determine file list based on dates
  source_files <- FileListFromDates(start_date=start_date, end_date=end_date)
  if(0 == length(source_files)) stop("No GDELT files available for the dates specified")
  
  # Ingest and filter local files
  for(this_file in LocalVersusRemote(filelist=source_files, local_folder=local_folder)$local) {
    if(FALSE == IsValidGDELT(f=this_file, local_folder=local_folder)) {
      # remove the offending file; it'll be downloaded in the later loop
      file.remove(paste(local_folder, "/", this_file, sep=""))
      next
    }
    new_data <- GdeltZipToDataframe(f=paste(local_folder, "/", this_file, sep=""),
                                    daily=grepl("export.CSV", this_file, fixed=TRUE),
                                    verbose=verbose)
    if(!missing(row_filter) | !missing(...)) new_data <- FilterGdeltDataframe(x=new_data,
                                                          verbose=verbose,
                                                          row_filter=row_filter,
                                                          ...=...)
    if(out_initialized) {
      out <- rbind(out, new_data)
    } else {
      out <- new_data
      out_initialized <- TRUE
    }
  }
  
  # Download, ingest, and filter remote files
  for(this_file in LocalVersusRemote(filelist=source_files, local_folder=local_folder)$remote) {
    download_result <- DownloadGdelt(f=this_file,
                                     local_folder=local_folder,
                                     max_local_mb=max_local_mb,
                                     data_url_root=data_url_root,
                                     verbose=verbose)
    if(FALSE == download_result) {
      stop("Unable to download file ", this_file, ". Please try again. If you get this result again, the file might not be available on the server.")
    }
    if(FALSE == IsValidGDELT(f=this_file, local_folder=local_folder)) {
      # try again
      download_result <- DownloadGdelt(f=this_file,
                                       local_folder=local_folder,
                                       max_local_mb=max_local_mb,
                                       data_url_root=data_url_root,
                                       verbose=verbose)
      if(FALSE == IsValidGDELT(f=this_file, local_folder=local_folder)) {
        stop("Unable to verify the integrity of ", this_file)
      }
    }
    
    new_data <- GdeltZipToDataframe(f=paste(local_folder, "/", this_file, sep=""),
                                    daily=grepl("export.CSV", this_file, fixed=TRUE),
                                    verbose=verbose)
    if(!missing(row_filter)) new_data <- FilterGdeltDataframe(x=new_data,
                                                          verbose=verbose,
                                                          row_filter=row_filter,
                                                          ...=...)
    if(out_initialized) {
      out <- rbind(out, new_data)
    } else {
      out <- new_data
      out_initialized <- TRUE
    }
  }
  
  # Filter one more time on dates
  start_date_numeric <- as.numeric(strftime(dateParse(start_date), format="%Y%m%d"))
  end_date_numeric <- as.numeric(strftime(dateParse(end_date), format="%Y%m%d"))
  out <- out[out$SQLDATE >= start_date_numeric & out$SQLDATE <= end_date_numeric,]
  
  return(out)
}
