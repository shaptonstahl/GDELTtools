test_that("Removes files to get total in path under max_local_mb", {
  path <- paste(tempdir(), "/rtest", sep="")
  dir.create(path=path, showWarnings=FALSE, recursive=TRUE)
  saveRDS(rep(1, 10e6), paste(path, "/oldest.rds", sep=""))
  saveRDS(rep(1, 10e6), paste(path, "/middle.rds", sep=""))
  saveRDS(rep(1, 10e6), paste(path, "/newest.rds", sep=""))
  fi <- FileInfo(path)
  
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("middle.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))

  expect_true(EnforceMaxDownloads(10, path))
  fi <- FileInfo(path)
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("middle.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))
  
  expect_true(EnforceMaxDownloads(.25, path, "oldest.rds"))
  fi <- FileInfo(path)
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_false(any(grepl("middle.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))

  expect_false(EnforceMaxDownloads(.15, path, c("oldest.rds","newest.rds")))
  fi <- FileInfo(path)
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))
  
  expect_true(EnforceMaxDownloads(.15, path))
  fi <- FileInfo(path)
  expect_false(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))
  
  unlink(path, recursive = TRUE)
})
