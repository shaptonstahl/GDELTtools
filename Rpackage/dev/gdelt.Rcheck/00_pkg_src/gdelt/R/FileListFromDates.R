## Segment of code that returns list of all necessary GDELT files to cover the time span specified

# assume you have startdate and enddate with day, month, year

## Need to make sure startdate and enddate are in range 1979-01-01 to today and enddate is later than startdate.  If not, force into acceptable range or return an error.



filelists<-function(startdate, enddate){
  
startdate<-as.Date(startdate)
enddate<-as.Date(enddate)
  
## PART A
## make the yyyy list for 1979 through 2005

A<-NULL
if(startdate<"2006-01-01"){
  if(enddate<"2006-01-01"){ 
    ## set enddate to the end of the year
    startdate<-as.Date(paste(format(startdate, "%Y"),"-01-01", sep=""))
    
    ## make list of years
    A<-c(format(seq(startdate,to=enddate,by='1 year'), "%Y"))
  }
  else{
    ## make list of year
    A<-c(format(seq(startdate,to=as.Date("2005-12-31"),by='1 year'), "%Y"))
  }
}

# PART B
# Make the yyyymm list for 2006 through April 2013

B<-NULL
if(startdate<"2013-04-01"&enddate>="2006-01-01"){ #establishes that span covers some part of 2006 - April 2013
  
  # determine bounds of span and make sequence accordingly
  if(startdate<"2006-01-01"){
    if(enddate<"2013-04-01"){
      B<-c(format(seq(as.Date("2006-01-01"),to=enddate,by='1 month'), "%Y%m"))
    }
    else{
      B<-c(format(seq(as.Date("2006-01-01"),to=as.Date("2013-03-31"),by='1 month'), "%Y%m"))
    }
  }else{
    # since using start date, set start date to the first of the month
    startdate<-as.Date(startdate-as.numeric(format(startdate, "%d"))+1)
    
    if(enddate<"2013-04-01"){
      B<-c(format(seq(startdate,to=enddate,by='1 month'), "%Y%m"))
    }
    else{
      B<-c(format(seq(startdate,to=as.Date("2013-03-31"),by='1 month'), "%Y%m"))
    }
  }
}

# Join lists from part A and part B
historic<-sub("$", ".zip", c(A,B))

# PART C
# Make list of yyyymmdd post April 2013

if(enddate>="2013-04-01"){ #establishes that span covers some part of post April 2013
  
  # determine bounds of span and make sequence accordingly
  if(startdate<"2013-04-01"){
    C<-c(format(seq(as.Date("2013-04-01"),to=enddate,by='1 day'), "%Y%m%d"))
  }
  else{
    C<-c(format(startdate,to=enddate,by='1 day'), "%Y%m%d")
  }
}
daily<-sub("$", ".export.CSV.zip", C )

return(list(historic = historic, daily = daily))

}

