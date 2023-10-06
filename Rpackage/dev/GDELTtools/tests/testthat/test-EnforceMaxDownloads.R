test_that("Removes files to get total in path under max_local_mb", {
  e_path <- file.path(path, "rtest")
  dir.create(path=e_path, showWarnings=FALSE, recursive=TRUE)
  
  saveRDS(rep(1, 10e6), file.path(e_path, "oldest.rds"))
  saveRDS(rep(1, 10e6), file.path(e_path, "middle.rds"))
  saveRDS(rep(1, 10e6), file.path(e_path, "newest.rds"))

  fi <- dir(e_path)
  expect_true(any("oldest.rds" %in% fi))
  expect_true(any("middle.rds" %in% fi))
  expect_true(any("newest.rds" %in% fi))

  expect_true(EnforceMaxDownloads(10, e_path))
  fi <- dir(e_path)
  expect_true(any("oldest.rds" %in% fi))
  expect_true(any("middle.rds" %in% fi))
  expect_true(any("newest.rds" %in% fi))
  
  expect_true(EnforceMaxDownloads(.25, e_path, "oldest.rds"))
  fi <- dir(e_path)
  expect_true(any("oldest.rds" %in% fi))
  expect_false(any("middle.rds" %in% fi))
  expect_true(any("newest.rds" %in% fi))
  
  expect_false(EnforceMaxDownloads(.15, e_path, c("oldest.rds","newest.rds")))
  fi <- dir(e_path)
  expect_true(any("oldest.rds" %in% fi))
  expect_true(any("newest.rds" %in% fi))
  
  expect_true(EnforceMaxDownloads(.15, e_path))
  fi <- dir(e_path)
  expect_false(any("oldest.rds" %in% fi))
  expect_true(any("newest.rds" %in% fi))
  
  unlink(e_path, recursive = TRUE)
})
