# create a temp folder for downloading files during the automated tests
path <- tempdir()

unzip(testthat::test_path("fixtures","v1_event_file_list.txt.zip"), 
      exdir=path,
      junkpaths = TRUE)
unzip(testthat::test_path("fixtures","v1_event_md5sums.txt.zip"), 
      exdir=path,
      junkpaths = TRUE)
unzip(testthat::test_path("fixtures","v1_gkg_file_list.txt.zip"), 
      exdir=path,
      junkpaths = TRUE)
unzip(testthat::test_path("fixtures","v1_gkg_md5sums.txt.zip"), 
      exdir=path,
      junkpaths = TRUE)
unzip(testthat::test_path("fixtures","v2_masterfilelist_english.txt.zip"), 
      exdir=path,
      junkpaths = TRUE)
