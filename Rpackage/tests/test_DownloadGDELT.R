# Test DownloadGDELT, IsValidGDELT, GetAllOfGDELT, 

test.local.folder <- "c:/gdeltdata"

DownloadGdelt(f="1979.zip", local.folder=test.local.folder)
DownloadGdelt(f="20140215.export.CSV.zip", local.folder=test.local.folder)

IsValidGDELT(f="1979.zip", local.folder=test.local.folder)
IsValidGDELT(f="20140215.export.CSV.zip", local.folder=test.local.folder)

GetAllOfGDELT(local.folder=test.local.folder)  # stop if desired before completion
# Test responding "n"
# Test responding "y"