#' Scale event counts
#'
#' Scale event counts based on the unit of analysis.
#' 
#' @aliases NormEventCounts
#' @param data data.frame, a GDELT dataframe.
#' @param include.events character, vector with CAMEO event codes to include in the count.
#' @param condition list, any additional conditions for coding the event count, such as, e.g., restricting to particular actor codes. this needs to refer to dataframe declared above.
#' @param unit.analysis character, default is country.day; other options: country.day, country.month, country.year, day, month, year
#' @param var.name, character, base name for the new count variables
#' @return data.frame
#' @export
#' @details
#' For \code{unit.analysis}, day and country-day put out a dataset where date is of class \sQuote{date}.
#  All other options put out a dataset where year or month is integer (this needs to be unified in a later version).
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{http://gdelt.utdallas.edu/}
#' @author 
#' \tabular{ll}{
#'   Timo Thoms \tab \email{othoms@@princeton.edu}\cr
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#' }
#' @examples
#' \dontrun{
#' new.data.frame <- NormEventCounts(data = GDELT.subset.data, 
#'   include.events = c(141, 145, 140, 143, 142, 144, 1412, 1413, 1411, 1431, 1414), 
#'   condition = (GDELT.subset.data $ActionGeo_CountryCode == "AF"), 
#'   unit.analysis = "country.year", 
#'   var.name = "protest")}
NormEventCounts <- function(data, 
                            include.events, 
                            condition, 
                            unit.analysis, 
                            var.name){
  # files with total event counts are included in the package, but these could be stored on a server and downloaded as needed 
  if(missing(data)) stop("No dataframe!")
  if(missing(include.events)) stop("You must declare event codes to subset on!")
  if(missing(unit.analysis)) unit.analysis <- "country.day" # default
  if(!(unit.analysis %in% c("country.day", "country.month", "country.year", "day", "month", "year"))) stop("Not a valid unit of analysis! \nChoose from: country.day, country.month, country.year, day, month, year")
  if(missing(condition)) condition <- TRUE # no further restrictions on the dataset
  
  data <- subset(data, subset = (condition & EventCode %in% include.events)) # keep only desired events and impose restrictions
  
  if(unit.analysis == "country.day"){ # this is the default (see here for code annotations)
    data$count <- tapply(data$EventCode, paste(data$ActionGeo_CountryCode, data$SQLDATE), length)[paste(data$ActionGeo_CountryCode, data$SQLDATE)] # code event counts
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "daily_country.RData")) # get total counts 
    data <- merge(data, daily.country, by.x = c("ActionGeo_CountryCode", "SQLDATE"), by.y = c("country", "day"), all.x = TRUE) # merge the two together
    data$norm.count <- data$count/data$total # code the normalized count
    range <- as.Date(as.character(range(data$SQLDATE)), format = "%Y%m%d") # get the first and last date in the data
    days <- format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 day"), "%Y%m%d") # create a vector with all dates in the range
    data <- unique(subset(data, select = c(ActionGeo_CountryCode, SQLDATE, count, norm.count))) # only keep the columns needed & subset to unique country-days
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "country_codes.RData")) # get the country FIPS 10-4 codes for merging
    complete <- expand.grid(country = countries$fips104, date = days) # create complete time-series per country
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = "")) # create new names based on user input
    names(data) <- c("country", "date", new.vars) # assign column names
    data <- merge(complete, data, by = c("country", "date"), all.x = TRUE) # merge data into complete time-series
    data[is.na(data[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0) # code NAs as 0s
    data[, "date"] <- as.Date(data[, "date"], "%Y%m%d") # coerce into date format
  } 
  if(unit.analysis == "country.month"){
    data$count <- tapply(data$EventCode, paste(data$ActionGeo_CountryCode, data$MonthYear), length)[paste(data$ActionGeo_CountryCode, data$MonthYear)]
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "monthly_country.RData"))
    data <- merge(data, monthly.country, by.x = c("ActionGeo_CountryCode", "MonthYear"), by.y = c("country", "month"), all.x = TRUE)
    data$norm.count <- data$count/data$total
    range <- as.Date(as.character(range(data$SQLDATE)), format = "%Y%m%d") # get the first and last date in the data
    months <- as.integer(format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 month"), "%Y%m"))
    data <- unique(subset(data, select = c(ActionGeo_CountryCode, MonthYear, count, norm.count)))
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "country_codes.RData"))
    complete <- expand.grid(country = countries$fips104, month = months) 
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(data) <- c("country", "month", new.vars) 
    data <- merge(complete, data, by = c("country", "month"), all.x = TRUE)
    data[is.na(data[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  } 
  if(unit.analysis == "country.year"){ 
    data$count <- tapply(data$EventCode, paste(data$ActionGeo_CountryCode, data$Year), length)[paste(data$ActionGeo_CountryCode, data$Year)]
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "yearly_country.RData"))
    data <- merge(data, yearly.country, by.x = c("ActionGeo_CountryCode", "Year"), by.y = c("country", "year"), all.x = TRUE)
    data$norm.count <- data$count/data$total
    range <- range(data$Year)
    if(range[2] > 2012) cat("Normalized counts only available until 2012!")
    data <- unique(subset(data, select = c(ActionGeo_CountryCode, Year, count, norm.count)))
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "country_codes.RData"))
    complete <- expand.grid(country = countries$fips104, year = range[1]:range[2])
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(data) <- c("country", "year", new.vars)
    data <- merge(complete, data, by = c("country", "year"), all.x = TRUE) 
    data[is.na(data[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  } 
  if(unit.analysis == "day"){
    data$count <- tapply(data$EventCode, data$SQLDATE, length)[as.factor(data$SQLDATE)]
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "daily.RData"))
    data <- merge(data, daily, by.x = "SQLDATE", by.y = "day", all.x = TRUE)		
    data$norm.count <- data$count/data$total
    range <- as.Date(as.character(range(data$SQLDATE)), format = "%Y%m%d")
    days <- format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 day"), "%Y%m%d")
    data <- unique(subset(data, select = c(SQLDATE, count, norm.count)))
    complete <- expand.grid(date = days) 
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(data) <- c("date", new.vars)
    data <- merge(complete, data, by = "date", all.x = TRUE)
    data[is.na(data[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
    data[, "date"] <- as.Date(data[, "date"], "%Y%m%d")
  } 
  if(unit.analysis == "month"){
    data$count <- tapply(data$EventCode, data$SQLDATE, length)[as.factor(data$SQLDATE)]
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "monthly.RData"))
    data <- merge(data, monthly, by.x = "MonthYear", by.y = "month", all.x = TRUE)		
    data$norm.count <- data$count/data$total
    range <- as.Date(as.character(range(data$SQLDATE)), format = "%Y%m%d")
    months <- as.integer(format(seq(as.Date(range[1]), to = as.Date(range[2]), by = "1 month"), "%Y%m"))
    data <- unique(subset(data, select = c(MonthYear, count, norm.count)))
    complete <- expand.grid(month = months) 
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(data) <- c("month", new.vars)
    data <- merge(complete, data, by = "month", all.x = TRUE)
    data[is.na(data[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  } 
  if(unit.analysis == "year"){
    data$count <- tapply(data$EventCode, data$Year, length)[as.factor(data$Year)]
    load(unz(paste(getwd(), "/RData.zip", sep = ""), "yearly.RData"))
    data <- merge(data, yearly, by.x = "Year", by.y = "year", all.x = TRUE)		
    data$norm.count <- data$count/data$total
    range <- range(data$Year)
    if(range[2] > 2012) cat("Normalized counts only available until 2012!")
    data <- unique(subset(data, select = c(Year, count, norm.count)))
    complete <- expand.grid(year = range[1]:range[2])
    new.vars <- c(paste(var.name, ".count", sep = ""), paste(var.name, ".norm", sep = ""))
    names(data) <- c("year", new.vars)
    data <- merge(complete, data, by = "year", all.x = TRUE)
    data[is.na(data[, paste(var.name, ".norm", sep = "")]), new.vars] <- c(0, 0)
  }
  row.names(data) <- 1:nrow(data)
  return(data)
}
