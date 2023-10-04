# Test DownloadGDELT, IsValidGDELT, GetAllOfGDELT, 

test.local.folder <- "c:/gdeltdata"

DownloadGdelt(f="1979.zip", local.folder=test.local.folder)
DownloadGdelt(f="20140215.export.CSV.zip", local.folder=test.local.folder)

IsValidGDELT(f="1979.zip", local.folder=test.local.folder)
IsValidGDELT(f="20140215.export.CSV.zip", local.folder=test.local.folder)

GetAllOfGDELT(local.folder=test.local.folder)  # stop if desired before completion
# Test responding "n"
# Test responding "y"

# Test FileListFromDates
FileListFromDates(start.date="1979-06-04", end.date="2001-09-11")
FileListFromDates(start.date="1982-12-31", end.date="2006-11-15")
FileListFromDates(start.date="2001-03-31", end.date="2013-04-15")
FileListFromDates(start.date="2012-03-31", end.date="2013-04-15")
FileListFromDates(start.date="2013-06-31", end.date="2013-04-15")  # return error
FileListFromDates(start.date="2013-12-31", end.date=Sys.Date()-1)

# Test warnings for missing dates
test.filter <- list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US")
test <- GetGDELT(start.date="2014-01-22", end.date="2014-01-22", filter=test.filter,local.folder=test.local.folder)
test <- GetGDELT(start.date="2014-01-23", end.date="2014-01-25", filter=test.filter,local.folder=test.local.folder)
test <- GetGDELT(start.date="2014-01-22", end.date="2014-01-23", filter=test.filter,local.folder=test.local.folder)
test <- GetGDELT(start.date="2014-01-25", end.date="2014-01-26", filter=test.filter,local.folder=test.local.folder)
test <- GetGDELT(start.date="2014-01-26", end.date="2014-01-26", filter=test.filter,local.folder=test.local.folder)
