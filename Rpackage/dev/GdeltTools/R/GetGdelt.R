#' Download and subset GDELT V1 event data
#'
#' Download the GDELT V1 Event files necessary for a data set, import them, filter on various criteria, and return a data.frame. 
#' 
#' @aliases GetGDELT
#' @param start_date character, earliest date to include in "YYYY-MM-DD" format.
#' @param end_date character, latest date to include in "YYYY-MM-DD" format.
#' @param row_filter <data-masking> Row selection. Expressions that return a logical value, and are defined in terms of the variables in GDELT. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#' @param ... <tidy-select>, Column selection. This takes the form of one or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.
#' @param local_folder character, where downloaded files will be saved.
#' @param max_local_mb numeric, the maximum size in MB of the downloaded files that will be retained.
#' @param version integer, URL for the folder with GDELT data files.
#' @param data_type character, one of events, gkg, gkgcounts, or mentions.
#' @param timeout integer, maximum number of seconds allowed for downloading each file.
#' @param verbose logical, if TRUE then indications of progress will be displayed.
#' @return data.frame
#' @export
#' @details
#' 
#' Dates are parsed with \code{guess_datetime} in the datetimeutils package. 
#' The recommended format is "YYYY-MM-DD".
#'
#' If a needed file has already been downloaded to \code{local_folder} then this 
#' file is used instead of being downloaded. This can greatly speed up future
#' downloads.
#' 
#' @section Filtering Results:
#' 
#' The \code{row_filter} is passed to \code{dplyr::filter}. This is a very flexible way to filter
#' the rows. It's well worth checking out the \code{dplyr::filter} documentation.
#' 
#' @section Selecting Columns:
#' 
#' The \code{...} is passed to \code{dplyr::select}. This is a very flexible way to choose
#' which columns to return. It's well worth checking out the \code{dplyr::select} documentation.
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
#' df1 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
#'                 local_folder="~/gdeltdata", version=1, data_type="events")
#' 
#' df2 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
#'                 row_filter=ActionGeo_CountryCode=="US",
#'                 local_folder="~/gdeltdata", version=1, data_type="events")
#' 
#' df3 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
#'                 row_filter=Actor2Geo_CountryCode=="RS" & NumArticles==2 & is.na(Actor1CountryCode), 
#'                 1:5,
#'                 local_folder="~/gdeltdata", version=1, data_type="events")
#' 
#' df4 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
#'                 row_filter=Actor2Code=="COP" | Actor2Code=="MED", 
#'                 contains("date"), starts_with("actor"),
#'                 local_folder="~/gdeltdata", version=1, data_type="events")
#'
#' }
GetGDELT <- function(start_date,
                     end_date=start_date,
                     row_filter,
                     ...,
                     local_folder,
                     max_local_mb=Inf,
                     version,
                     data_type=c("events","gkg","gkgcounts","mentions"),
                     timeout=300,
                     verbose=TRUE) {
  
  # Coerce ending slashes as needed
  
  local_folder <- StripTrailingSlashes(path.expand(local_folder))
  data_url_root <- DataURLRoot(version=version, data_type=data_type)
  # create the local_folder if is doesn't exist
  dir.create(local_folder, showWarnings=FALSE, recursive = TRUE)
  
  start_date <- strftime(guess_datetime(start_date, date.only=TRUE), format="%Y-%m-%d")
  end_date <- strftime(guess_datetime(end_date, date.only=TRUE), format="%Y-%m-%d")
  
  if(start_date <= "2014-01-25" & end_date >= "2014-01-23") warning("Your date range includes some or all of Feb 23-25, 2014. GDELT data is not available for these dates.")
  
  out_initialized <- FALSE
  
  # Determine file list based on dates
  source_files <- FileListFromDates(start_date=start_date, end_date=end_date)
  if(0 == length(source_files)) stop("No GDELT files available for the dates specified")
  
  # Ingest and filter local files
  for(this_file in LocalVersusRemote(filelist=source_files, local_folder=local_folder)$local) {
    if(FALSE == IsValidGDELT(x=this_file, local_folder=local_folder,
                             version=version, data_type=data_type,
                             timeout=timeout)) {
      # remove the offending file; it'll be downloaded in the later loop
      file.remove(paste(local_folder, "/", this_file, sep=""))
      next
    }
    new_data <- GdeltZipToDataframe(f=paste(local_folder, "/", this_file, sep=""),
                                    daily=grepl("export.CSV", this_file, fixed=TRUE),
                                    verbose=verbose)
    if(!missing(row_filter) | ...length() > 0) new_data <- FilterGdeltDataframe(x=new_data,
                                                                                verbose=verbose,
                                                                                row_filter={{row_filter}},
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
                                     max_local_mb=Inf,
                                     version=version,
                                     data_type=data_type,
                                     verbose=TRUE,
                                     timeout=timeout)
    if(FALSE == download_result) {
      stop("Unable to download file ", this_file, ". Please try again. If you get this result again, the file might not be available on the server.")
    }
    if(FALSE == IsValidGDELT(x=this_file, local_folder=local_folder,
                             version=version, data_type=data_type,
                             timeout=timeout)) {
      # try again
      download_result <- DownloadGdelt(f=this_file,
                                       local_folder=local_folder,
                                       max_local_mb=Inf,
                                       version=version,
                                       data_type=data_type,
                                       verbose=TRUE,
                                       timeout=timeout)
      if(FALSE == IsValidGDELT(x=this_file, local_folder=local_folder,
                               version=version, data_type=data_type,
                               timeout=timeout)) {
        stop("Unable to verify the integrity of ", this_file)
      }
    }
    
    new_data <- GdeltZipToDataframe(f=paste(local_folder, "/", this_file, sep=""),
                                    daily=grepl("export.CSV", this_file, fixed=TRUE),
                                    verbose=verbose)
    if(!missing(row_filter) | ...length() > 0) new_data <- FilterGdeltDataframe(x=new_data,
                                                                                verbose=verbose,
                                                                                row_filter={{row_filter}},
                                                                                ...=...)
    if(out_initialized) {
      out <- rbind(out, new_data)
    } else {
      out <- new_data
      out_initialized <- TRUE
    }
  }
  
  # Filter one more time on dates
  start_date_numeric <- as.numeric(strftime(guess_datetime(start_date, date.only=TRUE), format="%Y%m%d"))
  end_date_numeric <- as.numeric(strftime(guess_datetime(end_date, date.only=TRUE), format="%Y%m%d"))
  out <- out[out$SQLDATE >= start_date_numeric & out$SQLDATE <= end_date_numeric,]
  
  return(out)
}
