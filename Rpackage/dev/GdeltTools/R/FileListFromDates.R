## Segment of code that returns list of all necessary GDELT files to cover the time span specified

# input startdate and enddate in the form "yyyy-mm-dd"

FileListFromDates <- function(start_date, end_date=start_date){  
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if(end_date < start_date) stop("end_date cannot be before start_date")
  if(start_date < as.Date("1979-01-01")) stop("start_date cannot be before 1979")
  if(end_date > Sys.Date()) stop("end_date cannot be in the future")
  
  out <- character(0)
  
  ## PART A
  ## make the yyyy list for 1979 through 2005
  
  if(start_date < "2006-01-01"){
    start_year <- as.numeric(format(start_date, "%Y"))
    end_year <- min(c(2005,as.numeric(format(end_date, "%Y"))))
    out <- c(out, paste(start_year:end_year, ".zip", sep=""))
  }
  
  # PART B
  # Make the yyyymm list for 2006 through March 2013
  
  if(start_date < as.Date("2013-04-01") & end_date >= as.Date("2006-01-01")) {
    
    if( start_date < as.Date("2006-01-01") ) {
      b_start_date <- as.Date("2006-01-01")
    } else {
      b_start_date <- as.Date(paste(format(start_date, "%Y-%m-"), "01", sep=""))
    }
    
    if( end_date < as.Date("2013-04-01") ) {
      b_end_date <- end_date
    } else {
      b_end_date <- as.Date("2013-03-31")
    }
    out <- c(out, paste(format(seq(from=b_start_date, to=b_end_date, by="month"), "%Y%m"), 
                        ".zip", sep=""))
  }
  
  # PART C
  # Make list of yyyymmdd starting April 1, 2013
  if( end_date >= as.Date("2013-04-01") ){
    if( start_date < as.Date("2013-04-01") ) {
      c_start_date <- as.Date("2013-04-01")
    } else {
      c_start_date <- start_date
    }
    out <- c(out, paste(format(seq(from=c_start_date, to=end_date, by="day"), "%Y%m%d"), 
                        ".export.CSV.zip", sep=""))
  }
  
  # Remove dates GDELT just doesn't have
  out <- setdiff(out, c("20140123.export.CSV.zip","20140124.export.CSV.zip","20140125.export.CSV.zip"))
  
  return( out )
}
