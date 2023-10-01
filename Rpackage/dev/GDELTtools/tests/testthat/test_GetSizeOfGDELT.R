context("GetSizeOfGDELT")

test_that("returns valid size for GDELT", {
  expect_true(GetSizeOfGDELT(version=1, local_folder="~/gdeltdata", data_type="events") > 41.4)  # as of 9/25/2023
})
