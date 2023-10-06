
test_that("returns valid size for GDELT", {
  expect_true(GetSizeOfGDELT(version=1, data_type="events", 
                             local_folder=path) > 41.4)   # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=1, data_type="gkg", 
                             local_folder=path) > 123.8)   # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=1, data_type="gkgcounts", 
                             local_folder=path) > 15.9)    # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=2, data_type="events", 
                             local_folder=path) > .03)     # truncated metadata file
  expect_true(GetSizeOfGDELT(version=2, data_type="gkg", 
                             local_folder=path) > 2.1)     # truncated metadata file
  expect_true(GetSizeOfGDELT(version=2, data_type="mentions", 
                             local_folder=path) > .06)     # truncated metadata file
})

test_that("checks for invalid inputs", {
  expect_error(GetSizeOfGDELT(version=3, data_type="events", 
                              local_folder=path))
  expect_error(GetSizeOfGDELT(version=1, data_type="mentions", 
                              local_folder=path))
  expect_error(GetSizeOfGDELT(version=2, data_type="gkgcounts", 
                              local_folder=path))
})
