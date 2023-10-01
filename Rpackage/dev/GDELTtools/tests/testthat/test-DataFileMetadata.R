test_that("Downloads and returns metadata", {
  local_folder <- tempdir()
  
  v1_events    <- DataFileMetadata(version=1, data_type="events", local_folder=local_folder)
  v1_gkg       <- DataFileMetadata(version=1, data_type="gkg", local_folder=local_folder)
  v1_gkgcounts <- DataFileMetadata(version=1, data_type="gkgcounts", local_folder=local_folder)
  v2_events    <- DataFileMetadata(version=2, data_type="events", local_folder=local_folder)
  v2_gkg       <- DataFileMetadata(version=2, data_type="gkg", local_folder=local_folder)
  v2_mentions  <- DataFileMetadata(version=2, data_type="mentions", local_folder=local_folder)
  
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
})

test_that("Check for bad inputs", {
  local_folder <- tempdir()
  
  expect_error(DataFileMetadata(version=3, data_type="events", local_folder=local_folder))
  expect_error(DataFileMetadata(version=1, data_type="mentions", local_folder=local_folder))
  expect_error(DataFileMetadata(version=2, data_type="gkgcounts", local_folder=local_folder))
  expect_error(DataFileMetadata(version=1, data_type="clam", local_folder=local_folder))
  expect_error(DataFileMetadata(version=2, data_type="events"))
})
