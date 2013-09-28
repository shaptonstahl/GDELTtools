## Segment of code that returns list of all necessary GDELT files to cover the time span specified

# input startdate and enddate in the form "yyyy-mm-dd"

FileListFromDates <- function(startdate, enddate=startdate){  

startdate<-as.Date(startdate)
enddate<-as.Date(enddate)

if(enddate<startdate) stop("enddate cannot be before start date")
if(startdate<"1979-01-01") stop("startdate cannot be before 1979")
if(enddate>Sys.Date()) stop("enddate cannot be in the future")
   
## PART A
## make the yyyy list for 1979 through 2005

historic<-NULL
if(startdate<"2006-01-01"){
  if(enddate<"2006-01-01"){ 
    ## set enddate to the end of the year
    startdate<-as.Date(paste(format(startdate, "%Y"),"-01-01", sep=""))
    
    ## make list of years
    historic<-c(format(seq(startdate,to=enddate,by='1 year'), "%Y"))
  }
  else{
    ## make list of year
    historic<-c(format(seq(startdate,to=as.Date("2005-12-31"),by='1 year'), "%Y"))
  }
}

# PART B
# Make the yyyymm list for 2006 through April 2013

if(startdate<"2013-04-01"&enddate>="2006-01-01"){ #establishes that span covers some part of 2006 - April 2013
  
  # determine bounds of span and make sequence accordingly
  if(startdate<"2006-01-01"){
    if(enddate<"2013-04-01"){
      historic<-c(historic, format(seq(as.Date("2006-01-01"),to=enddate,by='1 month'), "%Y%m"))
    }
    else{
      historic<-c(historic, format(seq(as.Date("2006-01-01"),to=as.Date("2013-03-31"),by='1 month'), "%Y%m"))
    }
  }else{
    # since using start date, set start date to the first of the month
    startdate<-as.Date(startdate-as.numeric(format(startdate, "%d"))+1)
    
    if(enddate<"2013-04-01"){
      historic<-c(historic, format(seq(startdate,to=enddate,by='1 month'), "%Y%m"))
    }
    else{
      historic<-c(historic, format(seq(startdate,to=as.Date("2013-03-31"),by='1 month'), "%Y%m"))
    }
  }
}

# PART C
# Make list of yyyymmdd post April 2013
daily<-NULL

if(enddate>="2013-04-01"){ #establishes that span covers some part of post April 2013
  # determine bounds of span and make sequence accordingly
  if(startdate<"2013-04-01"){
    daily<-c(format(seq(as.Date("2013-04-01"),to=enddate,by='1 day'), "%Y%m%d"))
  }
  else{
    daily<-c(format(seq(startdate,to=enddate,by='1 day'), "%Y%m%d"))
  }
}

if(length(historic>0)){
  historic<-sub("$", ".zip", historic)
}

if(length(daily>0)){
  daily<-sub("$", ".export.CSV.zip", daily )
}

return(list(historic = historic, daily = daily))

}
