test_that("imports and parses dataframes from ziped files", {
  
  # v1 events - year file
  v1_events_year_fn <- "1979.zip"
  expect_true(DownloadIfMissing(v1_events_year_fn, 
                                url=paste(DataURLRoot(version=1, data_type="events"), 
                                          v1_events_year_fn, sep=""), 
                                local_folder=path))
  v1_events_year <- GdeltZipToDataframe(file_w_path=paste(path, "/", v1_events_year_fn, sep=""),
                                        version=1, data_type="events", v1_daily=FALSE, verbose=FALSE)
  expect_equal(nrow(v1_events_year), 430941)  
  expect_equal(ncol(v1_events_year), 58)
  remove(v1_events_year)
  
  # v1 events - day file
  v1_events_day_fn <- "20201114.export.CSV.zip"
  expect_true(DownloadIfMissing(v1_events_day_fn, 
                                url=paste(DataURLRoot(version=1, data_type="events"), 
                                          v1_events_day_fn, sep=""), 
                                local_folder=path))
  v1_events_day <- GdeltZipToDataframe(file_w_path=paste(path, "/", v1_events_day_fn, sep=""),
                                       version=1, data_type="events", v1_daily=TRUE, verbose=FALSE)
  expect_equal(nrow(v1_events_day), 16508)  
  expect_equal(ncol(v1_events_day), 58)
  remove(v1_events_day)
  
  # v1 gkg
  v1_gkg_fn <- "20201114.gkg.csv.zip"
  expect_true(DownloadIfMissing(v1_gkg_fn, 
                                url=paste(DataURLRoot(version=1, data_type="gkg"), 
                                          v1_gkg_fn, sep=""), 
                                local_folder=path))
  v1_gkg <- GdeltZipToDataframe(file_w_path=paste(path, "/", v1_gkg_fn, sep=""),
                                version=1, data_type="gkg", v1_daily=FALSE, verbose=FALSE)
  expect_equal(nrow(v1_gkg), 6137)  
  expect_equal(ncol(v1_gkg), 25)
  remove(v1_gkg)
  
  # v1 gkgcounts
  v1_gkgcounts_fn <- "20201114.gkgcounts.csv.zip"
  expect_true(DownloadIfMissing(v1_gkgcounts_fn, 
                                url=paste(DataURLRoot(version=1, data_type="gkgcounts"), 
                                          v1_gkgcounts_fn, sep=""), 
                                local_folder=path))
  v1_gkgcounts <- GdeltZipToDataframe(file_w_path=paste(path, "/", v1_gkgcounts_fn, sep=""),
                                      version=1, data_type="gkgcounts", v1_daily=FALSE, 
                                      verbose=FALSE)
  expect_equal(nrow(v1_gkgcounts), 3522)  
  expect_equal(ncol(v1_gkgcounts), 15)
  remove(v1_gkgcounts)
  
  # v2 events
  v2_events_fn <- "20201114124500.export.CSV.zip"
  expect_true(DownloadIfMissing(v2_events_fn, 
                                url=paste(DataURLRoot(version=2, data_type="events"), 
                                          v2_events_fn, sep=""), 
                                local_folder=path))
  v2_events <- GdeltZipToDataframe(file_w_path=paste(path, "/", v2_events_fn, sep=""),
                                   version=2, data_type="events", v1_daily=FALSE, verbose=FALSE)
  expect_equal(nrow(v2_events), 2248)  
  expect_equal(ncol(v2_events), 61)
  remove(v2_events)
  
  # v2 gkg
  v2_gkg_fn <- "20201114124500.gkg.csv.zip"
  expect_true(DownloadIfMissing(v2_gkg_fn, 
                                url=paste(DataURLRoot(version=2, data_type="gkg"), 
                                          v2_gkg_fn, sep=""), 
                                local_folder=path))
  v2_gkg <- GdeltZipToDataframe(file_w_path=paste(path, "/", v2_gkg_fn, sep=""),
                                version=2, data_type="gkg", v1_daily=FALSE, verbose=FALSE)
  expect_equal(nrow(v2_gkg), 1198)  
  expect_equal(ncol(v2_gkg), 53)
  remove(v2_gkg)
  
  # v2 gkg
  v2_mentions_fn <- "20201114124500.mentions.CSV.zip"
  expect_true(DownloadIfMissing(v2_mentions_fn, 
                                url=paste(DataURLRoot(version=2, data_type="mentions"), 
                                          v2_mentions_fn, sep=""), 
                                local_folder=path))
  v2_mentions <- GdeltZipToDataframe(file_w_path=paste(path, "/", v2_mentions_fn, sep=""),
                                version=2, data_type="mentions", v1_daily=FALSE, verbose=FALSE)
  expect_equal(nrow(v2_mentions), 4330)  
  expect_equal(ncol(v2_mentions), 17)
  remove(v2_mentions)
})
