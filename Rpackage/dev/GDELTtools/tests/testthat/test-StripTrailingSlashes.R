test_that("multiplication works", {
  expect_equal(StripTrailingSlashes("c:/Users/"), "c:/Users")
  expect_equal(StripTrailingSlashes("c:\\Users\\"), "c:\\Users")
  expect_equal(StripTrailingSlashes("c:/Users/\\"), "c:/Users")
})
