
test_that("returns valid size for GDELT", {
  expect_true(GetSizeOfGDELT(version=1, data_type="events", local_folder="~/gdeltdata") > 41.4)     # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=1, data_type="gkg", local_folder="~/gdeltdata") > 123.8)       # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=1, data_type="gkgcounts", local_folder="~/gdeltdata") > 15.9)  # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=2, data_type="events", local_folder="~/gdeltdata") > 29.6)     # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=2, data_type="gkg", local_folder="~/gdeltdata") > 2162.8)      # as of 9/25/2023
  expect_true(GetSizeOfGDELT(version=2, data_type="mentions", local_folder="~/gdeltdata") > 51.9)   # as of 9/25/2023
})

test_that("checks for invalid inputs", {
  expect_error(GetSizeOfGDELT(version=3, data_type="events", local_folder="~/gdeltdata"))
  expect_error(GetSizeOfGDELT(version=1, data_type="mentions", local_folder="~/gdeltdata"))
  expect_error(GetSizeOfGDELT(version=2, data_type="gkgcounts", local_folder="~/gdeltdata"))
})
