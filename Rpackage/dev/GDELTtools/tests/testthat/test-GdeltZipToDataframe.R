test_that("Get a dataframe from a zipped downloaded file", {
  path <- paste(tempdir(), "/rtest", sep="")
  dir.create(path=path, showWarnings=FALSE, recursive=TRUE)
  
  # V1 events - daily
  v1_events_file <- "20201114.export.CSV.zip"
  expect_true(
    DownloadIfMissing(file_name=v1_events_file,
                      url=paste(DataURLRoot(version=1, data_type="events"), 
                                v1_events_file, sep=""),
                      local_folder=path))
  v1_events <- GdeltZipToDataframe(f=v1_events_file, version=1, 
                                   data_type="events", daily=TRUE)
  expect_equal(ncol(v1_events), SOMENUMBEROFCOLUMNS)
  expect_true(nrow(v1_events) > 100)
  
  # V1 gkg
  
  # V1 gkgcounts
  
  # V2 events
  
  # V2 gkg
  
  # V2 mentions
  
  
  
  
  unlink(path, recursive = TRUE)
})
