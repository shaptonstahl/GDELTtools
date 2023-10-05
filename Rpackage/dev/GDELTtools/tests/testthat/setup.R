# create a temp folder for downloading files during the automated tests
path <- withr::local_tempdir()

DownloadIfMissing(file_name="v2_masterfilelist_english.txt", 
                  url="http://data.gdeltproject.org/gdeltv2/masterfilelist.txt",
                  local_folder=path, timeout=300)
