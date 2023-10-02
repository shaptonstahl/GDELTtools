test_that("Download and return metadata", {
  path <- paste(tempdir(), "/rtest", sep="")
  dir.create(path=path, showWarnings=FALSE, recursive=TRUE)
  
  v1_events    <- DataFileMetadata(version=1, data_type="events", local_folder=path)
  v1_gkg       <- DataFileMetadata(version=1, data_type="gkg", local_folder=path)
  v1_gkgcounts <- DataFileMetadata(version=1, data_type="gkgcounts", local_folder=path)
  v2_events    <- DataFileMetadata(version=2, data_type="events", local_folder=path)
  v2_gkg       <- DataFileMetadata(version=2, data_type="gkg", local_folder=path)
  v2_mentions  <- DataFileMetadata(version=2, data_type="mentions", local_folder=path)
  
  expect_true(nrow(v1_events) > 3900)
  expect_true(nrow(v1_gkg) > 3800)
  expect_true(nrow(v1_gkgcounts) > 3800)
  expect_true(nrow(v2_events) > 296000)
  expect_true(nrow(v2_gkg) > 296000)
  expect_true(nrow(v2_mentions) > 296000)
  
  expect_equal(names(v1_events), c("file_name", "size_bytes", "md5"))
  expect_equal(names(v1_gkg), c("file_name", "size_bytes", "md5"))
  expect_equal(names(v1_gkgcounts), c("file_name", "size_bytes", "md5"))
  expect_equal(names(v2_events), c("file_name", "size_bytes", "md5"))
  expect_equal(names(v2_gkg), c("file_name", "size_bytes", "md5"))
  expect_equal(names(v2_mentions), c("file_name", "size_bytes", "md5"))

  unlink(path, recursive = TRUE)
})

test_that("Check for bad inputs", {
  path <- paste(tempdir(), "/rtest", sep="")
  dir.create(path=path, showWarnings=FALSE, recursive=TRUE)
  
  expect_error(DataFileMetadata(version=3, data_type="events", local_folder=path))
  expect_error(DataFileMetadata(version=1, data_type="mentions", local_folder=path))
  expect_error(DataFileMetadata(version=2, data_type="gkgcounts", local_folder=path))
  expect_error(DataFileMetadata(version=1, data_type="clam", local_folder=path))
  expect_error(DataFileMetadata(version=2, data_type="events"))

  unlink(path, recursive = TRUE)
})
