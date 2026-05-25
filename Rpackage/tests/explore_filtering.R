# Experiment with filtering gkg data

devtools::load_all()
library(stringr)

path <- "~/gdeltdata"

v1_gkg_fn <- "20201114.gkg.csv.zip"
DownloadIfMissing(v1_gkg_fn, 
                  url=paste(DataURLRoot(version=1, data_type="gkg"), 
                            v1_gkg_fn, sep=""), 
                  local_folder=path)
v1_gkg <- GdeltZipToDataframe(file_w_path=file.path(path, v1_gkg_fn),
                              version=1, data_type="gkg", v1_daily=FALSE)
v1_gkg <- v1_gkg[sample(1:nrow(v1_gkg), 10),]

# DATE
v1_gkg$DATE == "2020-11-14"
v1_gkg$DATE < "2020-11-15"
v1_gkg$DATE %in% seq(from=as.Date("2020-01-01"), to=as.Date("2020-12-31"), by="day")

# NUMARTS
v1_gkg$NUMARTS == 1
v1_gkg$NUMARTS < 5
v1_gkg$NUMARTS %in% 1:10

# THEMES (SubList)
v1_gkg$THEMES == "LEADER" # no
sapply(v1_gkg$THEMES, function(x) "LEADER" %in% x) # yes
sapply(v1_gkg$THEMES, function(x) tolower("LEADER") %in% tolower(x)) # yes, case-insensitive
str_detect(v1_gkg$THEMES, fixed("LeAdEr", ignore_case=TRUE)) # yes, match substring, case-insensitive; warning
typeof(v1_gkg$THEMES)
sapply(v1_gkg$THEMES, function(x) 
  any(str_detect(x, pattern=fixed("LeAd", ignore_case=TRUE)))) # yes, match substring, case-insensitive

# LOCATIONS (SubTable)
str(v1_gkg$LOCATIONS[[1]]) # a data.frame
v1_gkg$LOCATIONS[2] # a list of length 1
v1_gkg$LOCATIONS[1]$GEO_FULLNAME # nope
as.vector(v1_gkg$LOCATIONS) # still a list
sapply(v1_gkg$LOCATIONS, function(x) 
  any(grepl("cali", x$GEO_FULLNAME, fixed=TRUE, ignore.case=TRUE))) # broken
grepl("cali", v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME, fixed=TRUE, ignore.case=TRUE)
any(grepl("miami", v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME, ignore.case = TRUE))
any(grepl("miami", v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME, ignore.case = TRUE, fixed=TRUE)) # broken
grepl("miami", v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME, ignore.case = TRUE, fixed=TRUE) # broken
any(grepl("miami", as.vector(v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME), ignore.case = TRUE, fixed=TRUE)) # broken
any(grepl("miami", unlist(v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME), ignore.case = TRUE, fixed=TRUE)) # broken
typeof(v1_gkg$LOCATIONS[[9]]$GEO_FULLNAME)
any(grepl("miami", list("this is miami", "this isn't"), ignore.case = TRUE, fixed=TRUE)) # broken
lapply(v1_gkg$LOCATIONS, function(x) 
  any(grepl("cali", x$GEO_FULLNAME, fixed=TRUE, ignore.case=TRUE))) # broken
lapply(v1_gkg$LOCATIONS, function(x) typeof(x$GEO_FULLNAME))
sapply(v1_gkg$LOCATIONS, function(x) 
  any(str_detect(x$GEO_FULLNAME, fixed("cali", ignore_case=TRUE)))) # yes, match substring, case-insensitive




m_v1_gkg <- read_delim(unz(file.path(path, v1_gkg_fn), 
                           unzip(file.path(path, v1_gkg_fn), list=TRUE)$Name[1]), 
                       delim="\t", col_types="ciccccccccc")
m_v1_gkg <- m_v1_gkg[sample(nrow(m_v1_gkg), 10),]
c(m_v1_gkg$LOCATIONS, as.character(NA))
x <- gsub(";", "\n", c(m_v1_gkg$LOCATIONS, as.character(NA)), fixed=TRUE)
lapply(x, function(y) {
  if(is.na(y)) return(NA)
  read_delim(I(y), delim="#", 
             col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                         "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID"),
             col_types="icccnnc")
})

SubTable(m_v1_gkg$LOCATIONS,
         col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                     "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID"),
         col_delim="#")


# INtermediate, slow version. SetDataframeColTypes is redundant.
SubTable <- function(field, 
                     col_names, 
                     col_types=paste(rep("c", length(col_names)), collapse=""), 
                     skip="",
                     row_delim=";", 
                     col_delim=",",
                     verbose=FALSE) {
  
  out <- lapply(gsub(row_delim, "\n", field, fixed=TRUE), 
                function(y) {
                  if(is.na(y)) return(NA)
                  suppressWarnings(
                    read_delim(I(y), delim=col_delim, 
                               col_names=col_names,
                               col_types=col_types,
                               progress=FALSE,
                               show_col_types=FALSE)
                  )
                })
  out <- SetDataframeColTypes(out, col_types=col_types, skip=skip, verbose=verbose)
  return(out)
}

ChildTable <- function(x, 
                       col_names, 
                       col_types=paste(rep("c", length(col_names)), collapse=""), 
                       skip="",
                       row_delim=";", 
                       col_delim=",",
                       verbose=FALSE) {
  # Given a delimited field and a primary key returns a dataframe with the
  # delimited field split up and the primary key repeated for join with the
  # source table
}


primary_keys <- CreatePrimaryKey(v1_gkg)
any(duplicated(primary_keys))

#######################################################

devtools::load_all()
library(stringr)
library(plyr)

v2_gkg_file_name <- "20201114124500.gkg.csv.zip"
path <- "~/gdeltdata"
file_w_path <- file.path(path, v2_gkg_file_name)

DownloadIfMissing(v2_gkg_file_name, 
                  url=paste(DataURLRoot(version=2, data_type="gkg"), 
                            v2_gkg_file_name, sep=""), 
                  local_folder=path)
# v2_gkg <- GdeltZipToDataframe(file_w_path=file_w_path, version=2, data_type="gkg")

v2_gkg_col_names <- c("GKGRECORDID","V2.1DATE","V2SOURCECOLLECTIONIDENTIFIER","V2SOURCECOMMONNAME",
                      "V2DOCUMENTIDENTIFIER","V1COUNTS","V2.1COUNTS","V1THEMES",
                      "V2ENHANCEDTHEMES","V1LOCATIONS","V2ENHANCEDLOCATIONS","V1PERSONS",
                      "V2ENHANCEDPERSONS","V1ORGANIZATIONS","V2ENHANCEDORGANIZATIONS","V1.5TONE",
                      "V2.1ENHANCEDDATES","V2GCAM","V2.1SHARINGIMAGE","V2.1RELATEDIMAGES",
                      "V2.1SOCIALIMAGEEMBEDS","V2.1SOCIALVIDEOEMBEDS","V2.1QUOTATIONS",
                      "V2.1ALLNAMES","V2.1AMOUNTS","V2.1TRANSLATIONINFO","V2EXTRASXML")
v2_gkg_col_types <- c("ccicccccccccccccccccccccccc")

out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]), 
                  delim="\t", col_names=v2_gkg_col_names,
                  col_types=v2_gkg_col_types)

out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))

GCAM_subtable <- ddply(out, "GDELTtools_key", .fun=function(subdf) {
  # subdf <- out[1,]
  x <- subdf$V2GCAM
  row_delim <- ","
  col_delim <- ":"
  col_names=c("DIMENSION","SCORE")
  col_types="cn"
  
  # q is a list of row vectors for the data.frame of locations
  # for this one row in the gkg dataset
  
  q <- str_split(str_split_1(x, row_delim), col_delim)
  # eliminate the few malformed rows, ones with more or less than
  # the expected number of elements
  q <- q[sapply(q, length)==length(col_names)]
  # if no rows are left, return NA instead of a data.frame
  if(length(q)==0) return( data.frame(DIMENSION=character(0), SCORE=numeric(0)) )
  else {
    # form the data.frame from the row vectors
    out_x <- ldply(q)
    # set the names for the data.frame
    names(out_x) <- col_names
    #out_x <- SetDataframeColTypes(out_x, col_types=col_types)
    return(out_x)
  }
})
