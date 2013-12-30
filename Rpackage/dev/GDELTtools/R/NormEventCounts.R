#' Scale event counts
#'
#' Scale event counts based on the unit of analysis.
#' 
#' @aliases NormEventCounts
#' @param x data.frame, a GDELT data.frame.
#' @param unit.analysis character, default is country.day; other options: country.day, country.month, country.year, day, month, year
#' @param var.name character, base name for the new count variables
#' @return data.frame
#' @export
#' @details
#' For \code{unit.analysis}, day and country-day put out a data set where date is of class \sQuote{date}.
#  All other options put out a data set where year or month is integer (this needs to be unified in a later version).
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{http://gdelt.utdallas.edu/}
#' @author 
#' \tabular{ll}{
#'   Oskar N.T. Thoms \tab \email{othoms@@princeton.edu}\cr
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#' }
#' @examples
#' \dontrun{
#' GDELT.subset.data <- GetGDELT("2013-06-01", "2013-06-07", allow.wildcards=TRUE,
#'   filter=list(ActionGeo_CountryCode=c("AF", "US"), EventCode="14*"),
#'   local.folder="~/gdeltdata")
#' GDELT.normed.data <- NormEventCounts(x = GDELT.subset.data, 
#'   unit.analysis="day", 
#'   var.name="protest")}
NormEventCounts <- function(x, 
                            unit.analysis, 
                            var.name){
  # files with total event counts are included in the package, but these could be stored on a server and downloaded as needed 
  if(missing(x)) stop("Must specify a data.frame")
  if(missing(unit.analysis)) stop("A unit of analysis must be specified")
  if(!(unit.analysis %in% c("country.day", "country.month", "country.year", "day", "month", "year"))) stop("Not a valid unit of analysis. Choose from: country.day, country.month, country.year, day, month, year")
  
  # load normalization data
  # THIS IS A KLUDGE THAT SHOULD BE FIXED
  e <- new.env()
  data(NormEventCountsData, envir=e)
  NormEventCountsData <- get("NormEventCountsData", envir=e)
  rm(e)
  
  if(unit.analysis == "country.day"){ # see here for code annotations
    x$count <- tapply(x$EventCode, paste(x$ActionGeo_CountryCode, x$SQLDATE), length)[paste(x$ActionGeo_CountryCode, x$SQLDATE)] # code event counts
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "daily_country.RData")) # get total counts 
    x <- merge(x, NormEventCountsData$daily.country, by.x = c("ActionGeo_CountryCode", "SQLDATE"), by.y = c("country", "day"), all.x = TRUE) # merge the two together
    x$norm.count <- x$count/x$total # code the normalized count
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d") # get the first and last date in the data
    days <- format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 day"), "%Y%m%d") # create a vector with all dates in the range
    x <- unique(subset(x, select = c("ActionGeo_CountryCode", "SQLDATE", "count", "norm.count"))) # only keep the columns needed & subset to unique country-days
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "country_codes.RData")) # get the country FIPS 10-4 codes for merging
    complete <- expand.grid(country = NormEventCountsData$countries$fips104, date = days) # create complete time-series per country
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = "")) # create new names based on user input
    names(x) <- c("country", "date", new.vars) # assign column names
    x <- merge(complete, x, by = c("country", "date"), all.x = TRUE) # merge data into complete time-series
    x[is.na(x[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0) # code NAs as 0s
    x[, "date"] <- as.Date(x[, "date"], "%Y%m%d") # coerce into date format
  } 
  if(unit.analysis == "country.month"){
    x$count <- tapply(x$EventCode, paste(x$ActionGeo_CountryCode, x$MonthYear), length)[paste(x$ActionGeo_CountryCode, x$MonthYear)]
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "monthly_country.RData"))
    x <- merge(x, NormEventCountsData$monthly.country, by.x = c("ActionGeo_CountryCode", "MonthYear"), by.y = c("country", "month"), all.x = TRUE)
    x$norm.count <- x$count/x$total
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d") # get the first and last date in the data
    months <- as.integer(format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 month"), "%Y%m"))
    x <- unique(subset(x, select = c("ActionGeo_CountryCode", "MonthYear", "count", "norm.count")))
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "country_codes.RData"))
    complete <- expand.grid(country = NormEventCountsData$countries$fips104, month = months) 
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(x) <- c("country", "month", new.vars) 
    x <- merge(complete, x, by = c("country", "month"), all.x = TRUE)
    x[is.na(x[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  } 
  if(unit.analysis == "country.year"){ 
    x$count <- tapply(x$EventCode, paste(x$ActionGeo_CountryCode, x$Year), length)[paste(x$ActionGeo_CountryCode, x$Year)]
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "yearly_country.RData"))
    x <- merge(x, NormEventCountsData$yearly.country, by.x = c("ActionGeo_CountryCode", "Year"), by.y = c("country", "year"), all.x = TRUE)
    x$norm.count <- x$count/x$total
    range <- range(x$Year)
    if(range[2] > 2012) cat("Normalized counts only available until 2012!")
    x <- unique(subset(x, select = c("ActionGeo_CountryCode", "Year", "count", "norm.count")))
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "country_codes.RData"))
    complete <- expand.grid(country = NormEventCountsData$countries$fips104, year = range[1]:range[2])
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(x) <- c("country", "year", new.vars)
    x <- merge(complete, x, by = c("country", "year"), all.x = TRUE) 
    x[is.na(x[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  } 
  if(unit.analysis == "day"){
    x$count <- tapply(x$EventCode, x$SQLDATE, length)[as.factor(x$SQLDATE)]
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "daily.RData"))
    x <- merge(x, NormEventCountsData$daily, by.x = "SQLDATE", by.y = "day", all.x = TRUE)		
    x$norm.count <- x$count/x$total
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d")
    days <- format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 day"), "%Y%m%d")
    x <- unique(subset(x, select = c("SQLDATE", "count", "norm.count")))
    complete <- expand.grid(date = days) 
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(x) <- c("date", new.vars)
    x <- merge(complete, x, by = "date", all.x = TRUE)
    x[is.na(x[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
    x[, "date"] <- as.Date(x[, "date"], "%Y%m%d")
  } 
  if(unit.analysis == "month"){
    x$count <- tapply(x$EventCode, x$SQLDATE, length)[as.factor(x$SQLDATE)]
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "monthly.RData"))
    x <- merge(x, NormEventCountsData$monthly, by.x = "MonthYear", by.y = "month", all.x = TRUE)		
    x$norm.count <- x$count/x$total
    range <- as.Date(as.character(range(x$SQLDATE)), format = "%Y%m%d")
    months <- as.integer(format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 month"), "%Y%m"))
    x <- unique(subset(x, select = c("MonthYear", "count", "norm.count")))
    complete <- expand.grid(month = months) 
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(x) <- c("month", new.vars)
    x <- merge(complete, x, by = "month", all.x = TRUE)
    x[is.na(x[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  } 
  if(unit.analysis == "year"){
    x$count <- tapply(x$EventCode, x$Year, length)[as.factor(x$Year)]
    #load(unz(paste(getwd(), "/RData.zip", sep = ""), "yearly.RData"))
    x <- merge(x, NormEventCountsData$yearly, by.x = "Year", by.y = "year", all.x = TRUE)		
    x$norm.count <- x$count/x$total
    range <- range(x$Year)
    if(range[2] > 2012) cat("Normalized counts only available until 2012!")
    x <- unique(subset(x, select = c("Year", "count", "norm.count")))
    complete <- expand.grid(year = range[1]:range[2])
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(x) <- c("year", new.vars)
    x <- merge(complete, x, by = "year", all.x = TRUE)
    x[is.na(x[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  }
  row.names(x) <- 1:nrow(x)
  return(x)
}
