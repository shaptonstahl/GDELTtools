context("GetSizeOfGDELT")

test_that("returns valid size for GDELT", {
  expect_true(GetSizeOfGDELT() > 41.4)  # as of 9/25/2023
})
