test_that("Removes files to get total in path under max_local_mb", {
  e_path <- paste(path, "/rtest", sep="")
  dir.create(path=e_path, showWarnings=FALSE, recursive=TRUE)
  saveRDS(rep(1, 10e6), paste(e_path, "/oldest.rds", sep=""))
  saveRDS(rep(1, 10e6), paste(e_path, "/middle.rds", sep=""))
  saveRDS(rep(1, 10e6), paste(e_path, "/newest.rds", sep=""))
  fi <- FileInfo(e_path)
  
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("middle.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))

  expect_true(EnforceMaxDownloads(10, e_path))
  fi <- FileInfo(e_path)
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("middle.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))
  
  expect_true(EnforceMaxDownloads(.25, e_path, "oldest.rds"))
  fi <- FileInfo(e_path)
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_false(any(grepl("middle.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))

  expect_false(EnforceMaxDownloads(.15, e_path, c("oldest.rds","newest.rds")))
  fi <- FileInfo(e_path)
  expect_true(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))
  
  expect_true(EnforceMaxDownloads(.15, e_path))
  fi <- FileInfo(e_path)
  expect_false(any(grepl("oldest.rds", fi$file_name, fixed=TRUE)))
  expect_true(any(grepl("newest.rds", fi$file_name, fixed=TRUE)))
  
  unlink(e_path, recursive = TRUE)
})
