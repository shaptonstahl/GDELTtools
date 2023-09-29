#' Download data from the GDELT Stability Dashboard API to memory
#'
#' @aliases GetGDELTStability
#' @param location character, two-digit country code or four-digit ADM1 code (see below).
#' @param var_to_get character, variable to download (see below).
#' @param time_resolution character, either "day" or "15min".
#' @param smoothing numeric, integer number of time_resolution periods to smooth over.
#' @param num_days numeric, number of days of data to download.
#' @param multi_ADM1 logical, if TRUE then var_to_get will be downloaded for all ADM1 codes in the country (specified in location).
#' @return data.frame
#' @export
#' 
#' @section location:
#' This is a single location code, either from
#' \url{http://data.gdeltproject.org/blog/stability-dashboard-api/GEOLOOKUP-COUNTRY.TXT}
#' or
#' \url{http://data.gdeltproject.org/blog/stability-dashboard-api/GEOLOOKUP-ADM1.TXT} 
#' 
#' @section var_to_get: 
#' One of:
#' 
#' - "instability": This display a simple synthetic "instability" measure for a country offering a very basic, but insightful, view of the current level of conflict and instability involving it.  Currently it is calculated by summing the total number of QuadClass=MaterialConflict and EventRootCode=14(Protest) events together and dividing by the total number of all events worldwide monitored by GDELT in the same time period.  This yields a normalized view of instability.
#' 
#' - "conflict": Same as above, but only includes QuadClass=MaterialConflict, ignoring protest events.
#' 
#' - "protest": Same as above, but only includes EventRootCode=14, assessing only protest activity, but excluding all other kinds of conflict.
#' 
#' - "tone": Average Standard GDELT Tone of all articles mentioning the location at least twice in the article within the given timeframe.  This uses a very basic filter of requiring that an article mention the location at least twice anywhere in the article body, and assesses tone at the article level.  Currently only the Standard GDELT Tone emotion is available, but in the future we hope to integrate the entire array of GCAM emotions.  This variable can be especially insightful to spotting deteriorating situations where coverage of a country or area is turning increasingly negative, even if physical unrest has ceased or not yet begun.
#' 
#' - "artvolnorm": This tallies the total number of articles mentioning the location at least twice anywhere in the article, divided by the total number of articles monitored by GDELT in the given timeframe, offering a normalized view of attention being paid to the location regardless of any physical unrest or other activity occurring there.  This variable offers a useful measure of changes in overall global "attention" being paid to a given location. 
#' 
#' @references
#' GDELT Stability Dashboard API
#' \url{https://blog.gdeltproject.org/announcing-the-gdelt-stability-dashboard-api-stability-timeline/}
#' @author 
#' \tabular{ll}{
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#' }
#' @examples
#' \dontrun{
#' ex1 <- GetGDELTStability(location="FR", 
#'                          var_to_get="tone", 
#'                          time_resolution="day", 
#'                          smoothing=1, 
#'                          num_days=10)
#' 
#' ex2 <- GetGDELTStability(location="IS", 
#'                          var_to_get="protest", 
#'                          time_resolution="15min", 
#'                          smoothing=3, 
#'                          num_days=1)
#' 
#' ex3 <- GetGDELTStability(location="AR", 
#'                          var_to_get="conflict", 
#'                          time_resolution="day", 
#'                          smoothing=1, 
#'                          num_days=10, 
#'                          multi_ADM1=TRUE)}
#'                           
GetGDELTStability <- function(location,
                              var_to_get=c("instabiliity","conflict","protest","tone","artvolnorm"),
                              time_resolution=c("day","15min"),
                              smoothing=1,
                              num_days=ifelse(time_resolution=="day", 180, 7),
                              multi_ADM1=FALSE) {
  op <- options()
  options(HTTPUserAgent=paste("GDELTtools v", packageVersion("GDELTtools"),
                              " in ", getOption("HTTPUserAgent"),
                              sep=""))
  get_url <- paste("https://api.gdeltproject.org/api/v1/dash_stabilitytimeline/dash_stabilitytimeline?",
                   "LOC=", str_to_upper(location), 
                   "&VAR=", str_to_lower(var_to_get),
                   "&OUTPUT=csv&TIMERES=", str_to_lower(time_resolution),
                   ifelse(multi_ADM1, "&MODE=multi", ""),
                   "&SMOOTH=", smoothing,
                   "&NUMDAYS=", num_days,
                   sep="")
  print(get_url)
  result <- read.csv(file=get_url, colClasses=c("character", "numeric"))
  options(op)
  
  if(str_to_lower(time_resolution) == "day") {
    names(result)[1] <- "Date"
    result$Date <- as.Date(result$Date, format="%Y%m%d")
  } else {
    names(result)[1] <- "Datetime"
    result$Datetime <- as.POSIXct(result$Datetime, format="%Y%m%d%H%M%S")
  }
  if(multi_ADM1) {
    # do nothing to second and other column names
  } else {
    names(result)[2] <- var_to_get
  }
  
  return(result)
}