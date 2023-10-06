# Download and save zipped copies of metadata files for testing without downloading
# The 90+MB metadata file for v2 is truncated.

ReadAndSaveZip <- function(remote_file, text_fn, local_dest, n_lines=-1L) {
  td <- tempdir()
  
  f <- base::url(remote_file)
  f_lines <- readLines(con=f, n=n_lines)
  close(f)
  
  f <- file(file.path(td, text_fn), open="w")
  writeLines(f_lines, con=f)
  close(f)
  
  zip(zipfile=file.path(local_dest, paste(text_fn, ".zip", sep="")),
      files=file.path(td, text_fn))
  unlink(file.path(td, text_fn))
}

ReadAndSaveZip(remote_file = "http://data.gdeltproject.org/events/filesizes",
               text_fn = "v1_event_file_list.txt",
               local_dest = testthat::test_path("fixtures"))
ReadAndSaveZip(remote_file = "http://data.gdeltproject.org/events/md5sums",
               text_fn = "v1_event_md5sums.txt",
               local_dest = testthat::test_path("fixtures"))
ReadAndSaveZip(remote_file = "http://data.gdeltproject.org/gkg/filesizes",
               text_fn = "v1_gkg_file_list.txt",
               local_dest = testthat::test_path("fixtures"))
ReadAndSaveZip(remote_file = "http://data.gdeltproject.org/gkg/md5sums",
               text_fn = "v1_gkg_md5sums.txt",
               local_dest = testthat::test_path("fixtures"))
ReadAndSaveZip(remote_file = "http://data.gdeltproject.org/gdeltv2/masterfilelist.txt",
               text_fn = "v2_masterfilelist_english.txt",
               local_dest = testthat::test_path("fixtures"),
               n_lines=900)



