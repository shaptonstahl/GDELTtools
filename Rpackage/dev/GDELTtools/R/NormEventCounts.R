#' Scale event counts
#'
#' Scale event counts based on the unit of analysis.
#' 
#' @aliases NormEventCounts
#' @param x data.frame, a GDELT data.frame.
#' @param unit_analysis character, default is country_day; other options: country_month, country_year, day, month, year
#' @param var_name character, base name for the new count variables
#' @return data.frame
#' @export
#' @details
#' For \code{unit_analysis}, day and country-day put out a data set where date
#' is of class \sQuote{date}.  All other options put out a data set where year
#' or month is integer (this needs to be unified in a later version).
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{https://www.gdeltproject.org/}
#' @author 
#' \tabular{ll}{
#'   Oskar N.T. Thoms \tab \email{othoms@@princeton.edu}\cr
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#'   John Beieler \tab \email{jub270@@psu.edu}\cr
#' }
#' @examples
#' \dontrun{
#' GDELT_subset_data <- GetGDELT("2013-06-01", "2013-06-07",
#'   (ActionGeo_CountryCode=="AF" | ActionGeo_CountryCode=="US") & EventCode>=140 & EventCode<150,
#'   local_folder="~/gdeltdata")
#' GDELT_normed_data <- NormEventCounts(x = GDELT_subset_data, 
#'   unit_analysis="day", 
#'   var_name="protest")}
NormEventCounts <- function(x, 
                            unit_analysis, 
                            var_name="norming_vars"){
  # files with total event counts are included in the package, but these could be stored on a server and downloaded as needed 
  if(missing(x)) stop("Must specify a data.frame")
  if(missing(unit_analysis)) stop("A unit of analysis must be specified")
  if(!(unit_analysis %in% c("country_day", 
                            "country_month", 
                            "country_year", 
                            "day", 
                            "month", 
                            "year"))) stop("Not a valid unit of analysis. Choose from: country_day, country_month, country_year, day, month, year")
  if(!all(c("EventCode","SQLDATE") %in% names(x))) stop("x must have EventCode and SQLDATE columns.")

  local_folder <- tempdir()

  cat("Downloading norm file\n")
  
  if(unit_analysis == "country_day"){ # see here for code annotations
    ##The pattern for downloading repeats for each unit of analysis
    #Set destination folder
    dest <- paste(local_folder, "/norm_counts_daily_country.csv", sep="")
    #Download data from the GDELT server at UMN
    dl_daily_country_data <- download.file(url="http://data.gdeltproject.org/normfiles/daily_country.csv", 
                                        destfile=dest, 
                                        quiet=FALSE)
    #Read in the downloaded CSV
    daily_country_data <- read.csv(dest, col.names=c("day", "country", "total"))

    # code event counts
    x$count <- tapply(x$EventCode, 
                      paste(x$ActionGeo_CountryCode, x$SQLDATE), length)[paste(x$ActionGeo_CountryCode, x$SQLDATE)]
    # merge the two together
    x <- merge(x, daily_country_data, 
               by.x = c("ActionGeo_CountryCode", "SQLDATE"), 
               by.y = c("country", "day"), 
               all.x = TRUE)
    
    # code the normalized count as (count of selected events in the country-day) / (all events in the country-day)
    x$norm_count <- x$count/x$total
    
    # get the first and last date in the data
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d")
    # create a vector with all dates in the range
    days <- format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 day"), "%Y%m%d")
    
    # only keep the columns needed & subset to unique country-days
    x <- unique(subset(x, select = c("ActionGeo_CountryCode", "SQLDATE", "count", "norm_count")))
    # create complete time-series per country
    complete <- expand.grid(country = unique(daily_country_data$country), date = days)
    # create new names based on user input
    new_vars <- c(paste(var_name, "_count", sep = ""), paste(var_name, "_norm", sep = ""))
    # assign column names
    names(x) <- c("country", "date", new_vars)
    # merge data into complete time-series
    x <- merge(complete, x, by = c("country", "date"), all.x = TRUE)
    # code NAs as 0s
    x[is.na(x[, paste(var_name, "_norm", sep = "")]), new_vars] <- c(0, 0)
    # coerce into date format
    x[, "date"] <- as.Date(x[, "date"], "%Y%m%d") 
  } 
  if(unit_analysis == "country_month"){
    dest <- paste(local_folder, "/norm_counts_monthly_country.csv", sep="")
    dl_monthly_country_data <- download.file(url="http://data.gdeltproject.org/normfiles/monthly_country.csv", 
                                        destfile=dest, 
                                        quiet=FALSE)
    monthly_country_data <- read.csv(dest, col.names=c("month", "country", "total"))

    x$count <- tapply(x$EventCode, paste(x$ActionGeo_CountryCode, x$MonthYear), length)[paste(x$ActionGeo_CountryCode, x$MonthYear)]
    x <- merge(x, monthly_country_data, by.x = c("ActionGeo_CountryCode", "MonthYear"), by.y = c("country", "month"), all.x = TRUE)
    x$norm_count <- x$count/x$total
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d") # get the first and last date in the data
    months <- as.integer(format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 month"), "%Y%m"))
    x <- unique(subset(x, select = c("ActionGeo_CountryCode", "MonthYear", "count", "norm_count")))
    complete <- expand.grid(country = unique(monthly_country_data$country), month = months) 
    new_vars <- c(paste(var_name, "_count", sep = ""), paste(var_name, "_norm", sep = ""))
    names(x) <- c("country", "month", new_vars) 
    x <- merge(complete, x, by = c("country", "month"), all.x = TRUE)
    x[is.na(x[, paste(var_name, "_norm", sep = "")]), new_vars] <- c(0, 0)
  } 
  if(unit_analysis == "country_year"){ 
    dest <- paste(local_folder, "/norm_counts_yearly_country.csv", sep="")
    dl_yearly_country_data <- download.file(url="http://data.gdeltproject.org/normfiles/yearly_country.csv", 
                                        destfile=dest, 
                                        quiet=FALSE)
    yearly_country_data <- read.csv(dest, col.names=c("year", "country", "total"))

    x$count <- tapply(x$EventCode, paste(x$ActionGeo_CountryCode, x$Year), length)[paste(x$ActionGeo_CountryCode, x$Year)]
    x <- merge(x, yearly_country_data, by.x = c("ActionGeo_CountryCode", "Year"), by.y = c("country", "year"), all.x = TRUE)
    x$norm_count <- x$count/x$total
    range <- range(x$Year)
    x <- unique(subset(x, select = c("ActionGeo_CountryCode", "Year", "count", "norm_count")))
    complete <- expand.grid(country = unique(yearly_country_data$country), year = range[1]:range[2])
    new_vars <- c(paste(var_name, "_count", sep = ""), paste(var_name, "_norm", sep = ""))
    names(x) <- c("country", "year", new_vars)
    x <- merge(complete, x, by = c("country", "year"), all.x = TRUE) 
    x[is.na(x[, paste(var_name, "_norm", sep = "")]), new_vars] <- c(0, 0)
  } 
  if(unit_analysis == "day"){
    dest <- paste(local_folder, "/norm_counts_daily.csv", sep="")
    dl_daily_data <- download.file(url="http://data.gdeltproject.org/normfiles/daily.csv", 
                                        destfile=dest, 
                                        quiet=FALSE)
    daily_data <- read.csv(dest, col.names=c("day", "total"))
    
    x$count <- tapply(x$EventCode, x$SQLDATE, length)[as.factor(x$SQLDATE)]
    x <- merge(x, daily_data, by.x = "SQLDATE", by.y = "day", all.x = TRUE)		
    x$norm_count <- x$count/x$total
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d")
    days <- format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 day"), "%Y%m%d")
    x <- unique(subset(x, select = c("SQLDATE", "count", "norm_count")))
    complete <- expand.grid(date = days) 
    new_vars <- c(paste(var_name, "_count", sep = ""), paste(var_name, "_norm", sep = ""))
    names(x) <- c("date", new_vars)
    x <- merge(complete, x, by = "date", all.x = TRUE)
    x[is.na(x[, paste(var_name, "_norm", sep = "")]), new_vars] <- c(0, 0)
    x[, "date"] <- as.Date(x[, "date"], "%Y%m%d")
  } 
  if(unit_analysis == "month"){
    dest <- paste(local_folder, "/norm_counts_monthly.csv", sep="")
    dl_monthly_data <- download.file(url="http://data.gdeltproject.org/normfiles/monthly.csv", 
                                        destfile=dest, 
                                        quiet=FALSE)
    monthly_data <- read.csv(dest, col.names=c("month", "total"))

    x$count <- tapply(x$EventCode, x$SQLDATE, length)[as.factor(x$SQLDATE)]
    x <- merge(x, monthly_data, by.x = "MonthYear", by.y = "month", all.x = TRUE)
    x$norm_count <- x$count/x$total
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d")
    months <- as.integer(format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 month"), "%Y%m"))
    x <- unique(subset(x, select = c("MonthYear", "count", "norm_count")))
    complete <- expand.grid(month = months) 
    new_vars <- c(paste(var_name, "_count", sep = ""), paste(var_name, "_norm", sep = ""))
    names(x) <- c("month", new_vars)
    x <- merge(complete, x, by = "month", all.x = TRUE)
    x[is.na(x[, paste(var_name, "_norm", sep = "")]), new_vars] <- c(0, 0)
  } 
  if(unit_analysis == "year"){
    dest <- paste(local_folder, "/norm_counts_yearly.csv", sep="")
    dl_yearly_data <- download.file(url="http://data.gdeltproject.org/normfiles/yearly.csv", 
                                        destfile=dest, 
                                        quiet=FALSE)
    yearly_data <- read.csv(dest, col.names=c("year", "total"))

    x$count <- tapply(x$EventCode, x$Year, length)[as.factor(x$Year)]
    x <- merge(x, yearly_data, by.x = "Year", by.y = "year", all.x = TRUE)		
    x$norm_count <- x$count/x$total
    range <- range(x$Year)
    x <- unique(subset(x, select = c("Year", "count", "norm_count")))
    complete <- expand.grid(year = range[1]:range[2])
    new_vars <- c(paste(var_name, "_count", sep = ""), paste(var_name, "_norm", sep = ""))
    names(x) <- c("year", new_vars)
    x <- merge(complete, x, by = "year", all.x = TRUE)
    x[is.na(x[, paste(var_name, "_norm", sep = "")]), new_vars] <- c(0, 0)
  }
  
  row.names(x) <- NULL
  return(x)
}
