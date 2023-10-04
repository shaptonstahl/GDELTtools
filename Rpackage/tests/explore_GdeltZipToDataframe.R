# Explore data files to ensure openign to usable dataframes

library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(datetimeutils)

source("~/GitHub/GDELTtools/Rpackage/dev/GDELTtools/R/DataURLRoot.R")
source("~/GitHub/GDELTtools/Rpackage/dev/GDELTtools/R/LocalVersusRemote.R")
source("~/GitHub/GDELTtools/Rpackage/dev/GDELTtools/R/DownloadIfMissing.R")
source("~/GitHub/GDELTtools/Rpackage/dev/GDELTtools/R/ListAllGDELTFiles.R")
source("~/GitHub/GDELTtools/Rpackage/dev/GDELTtools/R/DataFileMetadata.R")

path <- "~/gdeltdata"

####################### v1 events #########################
gdelt_colNames <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED")
gdelt_colClasses <- c(GLOBALEVENTID="integer", SQLDATE="integer", MonthYear="integer", Year="integer", FractionDate="numeric", Actor1Code="character", Actor1Name="character", Actor1CountryCode="character", Actor1KnownGroupCode="character", Actor1EthnicCode="character", Actor1Religion1Code="character", Actor1Religion2Code="character", Actor1Type1Code="character", Actor1Type2Code="character", Actor1Type3Code="character", Actor2Code="character", Actor2Name="character", Actor2CountryCode="character", Actor2KnownGroupCode="character", Actor2EthnicCode="character", Actor2Religion1Code="character", Actor2Religion2Code="character", Actor2Type1Code="character", Actor2Type2Code="character", Actor2Type3Code="character", IsRootEvent="logical", EventCode="character", EventBaseCode="character", EventRootCode="character", QuadClass="integer", GoldsteinScale="numeric", NumMentions="integer", NumSources="integer", NumArticles="integer", AvgTone="numeric", Actor1Geo_Type="integer", Actor1Geo_FullName="character", Actor1Geo_CountryCode="character", Actor1Geo_ADM1Code="character", Actor1Geo_Lat="numeric", Actor1Geo_Long="numeric", Actor1Geo_FeatureID="integer", Actor2Geo_Type="integer", Actor2Geo_FullName="character", Actor2Geo_CountryCode="character", Actor2Geo_ADM1Code="character", Actor2Geo_Lat="numeric", Actor2Geo_Long="numeric", Actor2Geo_FeatureID="integer", ActionGeo_Type="integer", ActionGeo_FullName="character", ActionGeo_CountryCode="character", ActionGeo_ADM1Code="character", ActionGeo_Lat="numeric", ActionGeo_Long="numeric", ActionGeo_FeatureID="integer", DATEADDED="integer")
cbind(gdelt_colNames, gdelt_colClasses)

gdelt_year_col_types <- "iciincccccccccccccccccccclccciniiinicccnniicccnniicccnnic"

v1_events_file_day <- "20201114.export.CSV.zip"
v1_events_day <- read_delim(unz(paste(path, "/", v1_events_file_day, sep=""), 
                                unzip(paste(path, "/", v1_events_file_day, sep=""), 
                                      list=TRUE)$Name[1]),
                            col_names=c(gdelt_colNames, "SOURCEURL"), delim="\t",
                            col_types=paste(gdelt_year_col_types, "c", sep=""))

v1_events_file_year <- "1979.zip"
v1_events_year <- read_delim(unz(paste(path, "/", v1_events_file_year, sep=""), 
                                 unzip(paste(path, "/", v1_events_file_year, sep=""), 
                                       list=TRUE)$Name[1]),
                             col_names=gdelt_colNames, delim="\t",
                             col_types = gdelt_year_col_types)
v1_events_year$SOURCEURL <- as.character(NA)

v1_events <- rbind(v1_events_day, v1_events_year)
v1_events$SQLDATE <- as.Date(v1_events$SQLDATE, format="%Y%m%d")
v1_events$DATEADDED <- as.Date(v1_events$DATEADDED, format="%Y%m%d")

cbind(gdelt_colNames, gdelt_colClasses, t(v1_events_year[1,1:57]))

cbind(v1_events_year[1:15, 1:5],   v1_events_day[1:15,1:5])
cbind(v1_events_year[1:15, 6:10],  v1_events_day[1:15,6:10])
cbind(v1_events_year[1:15, 11:15], v1_events_day[1:15,11:15])
cbind(v1_events_year[1:15, 16:20], v1_events_day[1:15,16:20])
cbind(v1_events_year[1:15, 21:25], v1_events_day[1:15,21:25])
cbind(v1_events_year[1:15, 26:30], v1_events_day[1:15,26:30])
cbind(v1_events_year[1:15, 31:35], v1_events_day[1:15,31:35])
cbind(v1_events_year[1:15, 36:40], v1_events_day[1:15,36:40])
cbind(v1_events_year[1:15, 41:45], v1_events_day[1:15,41:45])
cbind(v1_events_year[1:15, 46:50], v1_events_day[1:15,46:50])
cbind(v1_events_year[1:15, 51:55], v1_events_day[1:15,51:55])
cbind(v1_events_year[1:15, 56:57], v1_events_day[1:15,56:58])

######################## v1 gkg ######################
v1_gkg_files <- ListAllGDELTFiles(version=1, data_type="gkg", local_folder="~/gdeltdata")
v1_gkg_file <- "20201114.gkg.csv.zip"

DownloadIfMissing(v1_gkg_file, 
                  url=paste(DataURLRoot(version=1, data_type="gkg"), v1_gkg_file, sep=""), 
                  local_folder=path) 
v1_gkg <- read_delim(unz(paste(path, "/", v1_gkg_file, sep=""), 
                                unzip(paste(path, "/", v1_gkg_file, sep=""), 
                                      list=TRUE)$Name[1]), 
                     delim="\t", col_types="ciccccccccc")
v1_gkg$DATE <- as.Date(v1_gkg$DATE, format="%Y%m%d")
head(v1_gkg)

# COUNTS
gkg_count_lengths <- sapply(lapply(v1_gkg$COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), length)
table(gkg_count_lengths)

gkg_counts <- ldply(.data=lapply(v1_gkg$COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
      .fun=function(x) as.data.frame(t(x)))
names(gkg_counts) <- paste("COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                        "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                        "GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                           sep="")
head(gkg_counts)
v1_gkg <- cbind(v1_gkg, gkg_counts)
v1_gkg <- select(v1_gkg, -c("COUNTS"))
head(v1_gkg)

v1_gkg$THEMES <- lapply(v1_gkg$THEMES, function(x) str_split(x, ";")[[1]])
v1_gkg$THEMES <- lapply(v1_gkg$THEMES, function(x) x[-length(x)])
head(v1_gkg)

which(sapply(v1_gkg$LOCATIONS,
             function(x) {
               length(sapply(str_split(x, ";")[[1]], 
                             function(y) str_split(y, "#")[[1]]))
             }, USE.NAMES = FALSE) %% 7 > 0)
v1_gkg$LOCATIONS <- lapply(v1_gkg$LOCATIONS, 
                           function(x) {
                             # q is a list of row vectors for the data.frame of locations
                             # for this one row in the gkg dataset
                             q <- lapply(str_split(x, ";")[[1]], function(y) str_split(y, "#")[[1]])
                             # eliminate the few malformed rows, ones with more or less than
                             # seven elements
                             q <- q[sapply(q, length)==7]
                             # if no rows are left, return NA instead of a data.frame
                             if(length(q)==0) return(NA)
                             else {
                               # form the data.frame from the row vectors
                               out_x <- ldply(q)
                               # set the names for the data.frame
                               names(out_x) <- c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                                 "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID")
                               return(out_x)
                             }
                           })
head(v1_gkg)

v1_gkg$PERSONS <- lapply(v1_gkg$PERSONS, function(x) str_split(x, ";")[[1]])

v1_gkg$ORGANIZATIONS <- lapply(v1_gkg$ORGANIZATIONS, function(x) str_split(x, ";")[[1]])

gkg_tone <- ldply(.data=v1_gkg$TONE, .fun=function(x) str_split(x, ",")[[1]])
names(gkg_tone) <- paste("TONE_", c("TONE","POS_SCORE","NEG_SCORE","POLARITY",
                                    "ACTIVITY_REF_DENSITY","SELF_GROUP_REF_DENSITY"),
                         sep="")
head(gkg_tone)
v1_gkg <- cbind(v1_gkg, gkg_tone)
v1_gkg <- select(v1_gkg, -c("TONE"))
head(v1_gkg)

v1_gkg$CAMEOEVENTIDS <- lapply(v1_gkg$CAMEOEVENTIDS, function(x) str_split(x, ",")[[1]])

v1_gkg$SOURCES <- lapply(v1_gkg$SOURCES, function(x) str_split(x, ";")[[1]])

v1_gkg$SOURCEURLS <- str_split(v1_gkg$SOURCEURLS, "<UDIV>")

#################### v1 gkgcounts ####################
v1_gkg_counts_files <- ListAllGDELTFiles(version=1, data_type="gkgcounts", local_folder="~/gdeltdata")
v1_gkgcounts_file <- "20201114.gkgcounts.csv.zip"

DownloadIfMissing(v1_gkgcounts_file, 
                  url=paste(DataURLRoot(version=1, data_type="gkgcounts"), 
                            v1_gkgcounts_file, sep=""), 
                  local_folder=path) 
v1_gkgcounts <- read_delim(unz(paste(path, "/", v1_gkgcounts_file, sep=""), 
                               unzip(paste(path, "/", v1_gkgcounts_file, sep=""), 
                                     list=TRUE)$Name[1]), 
                           delim="\t", col_types="cicicicccnniccc")
v1_gkgcounts$DATE <- as.Date(v1_gkgcounts$DATE, format="%Y%m%d")
head(v1_gkgcounts)

v1_gkgcounts$CAMEOEVENTIDS <- lapply(v1_gkgcounts$CAMEOEVENTIDS, function(x) str_split(x, ",")[[1]])

v1_gkgcounts$SOURCES <- lapply(v1_gkgcounts$SOURCES, function(x) str_split(x, ";")[[1]])

v1_gkgcounts$SOURCEURLS <- str_split(v1_gkgcounts$SOURCEURLS, "<UDIV>")

head(v1_gkgcounts[11:15])

##################### v2 events #######################
v2_event_files <- ListAllGDELTFiles(version=2, data_type="events", local_folder="~/gdeltdata")
v2_event_file <- "20201114000000.export.CSV.zip"
v2_events[grepl("20201114", v2_events$file_name, fixed=TRUE),]
# missing files from 2020-11-14
should_have_dt <- timegrid(from=earliest_dt, to=latest_dt, interval="15 min", exclude.weekends=FALSE, fromHHMMSS="000000", toHHMMSS="240000")
present_file_dt <- as.POSIXct(substring(v2_event_files, 1, 14), format="%Y%m%d%H%M%S")
sum(!(should_have_dt %in% present_file_dt))           # 5607 missing files
length(present_file_dt) / length(should_have_dt)      # 98.1% of files present

v2_event_file <- "20201114124500.export.CSV.zip"
DownloadIfMissing(v2_event_file, 
                  url=paste(DataURLRoot(version=2, data_type="events"), v2_event_file, sep=""), 
                  local_folder=path) 
gdelt_v2_event_col_types <- "iciincccccccccccccccccccclccciniiiniccccnnciccccnnciccccnnccc"
gdelt_v2_event_col_names <- c("GlobalEventID", "Day", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_ADM2Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_ADM2Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_ADM2Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED", "SOURCEURL")
nchar(gdelt_v2_event_col_types)
length(gdelt_v2_event_col_names)
v2_events <- read_delim(unz(paste(path, "/", v2_event_file, sep=""), 
                            unzip(paste(path, "/", v2_event_file, sep=""), 
                                  list=TRUE)$Name[1]), 
                        delim="\t", col_names=gdelt_v2_event_col_names,
                        col_types=gdelt_v2_event_col_types)
v2_events$Day <- as.Date(v2_events$Day, format="%Y%m%d")
v2_events$DATEADDED <- as.POSIXct(v2_events$DATEADDED, format="%Y%m%d%H%M%S", tz="UTC")
head(v2_events)

####################### v2.0 gkg ######################
v2_gkg_files <- ListAllGDELTFiles(version=2, data_type="gkg", local_folder="~/gdeltdata")
v20_gkg_file <- "???"










####################### v2.1 gkg ######################
v2_gkg_files <- ListAllGDELTFiles(version=2, data_type="gkg", local_folder="~/gdeltdata")
v2_gkg_file <- "20201114124500.gkg.csv.zip"
v2_gkg_col_names <- c("GKGRECORDID","V2.1DATE","V2SOURCECOLLECTIONIDENTIFIER","V2SOURCECOMMONNAME",
                      "V2DOCUMENTIDENTIFIER","V1COUNTS","V2.1COUNTS","V1THEMES",
                      "V2ENHANCEDTHEMES","V1LOCATIONS","V2ENHANCEDLOCATIONS","V1PERSONS",
                      "V2ENHANCEDPERSONS","V1ORGNIZATIONS","V2ENHANCEDORGNIZATIONS","V1.5TONE",
                      "V2.1ENHANCEDDATES","V2GCAM","V2.1SHARINGIMAGE","V2.1RELATEDIMAGES",
                      "V2.1SOCIALIMAGEEMBEDS","V2.1SOCIALVIDEOEMBEDS","V2.1QUOTATIONS",
                      "V2.1ALLNAMES","V2.1AMOUNTS","V2.1TRANSLATIONINFO","V2EXTRASXML")
v2_gkg_col_types <- c("ccicccccccccccccccccccccccc")
length(v2_gkg_col_names)
nchar(v2_gkg_col_types)

DownloadIfMissing(v2_gkg_file, 
                  url=paste(DataURLRoot(version=2, data_type="gkg"), v2_gkg_file, sep=""), 
                  local_folder=path) 
v2_gkg <- read_delim(unz(paste(path, "/", v2_gkg_file, sep=""), 
                         unzip(paste(path, "/", v2_gkg_file, sep=""), 
                               list=TRUE)$Name[1]), 
                     delim="\t", col_names=v2_gkg_col_names,
                     col_types=v2_gkg_col_types)
## V2.1DATE
v2_gkg$V2.1DATE <- as.Date(v2_gkg$V2.1DATE, format="%Y%m%d")
head(v2_gkg)

## V1COUNTS
v2_gkg_counts <- ldply(.data=lapply(v2_gkg$V1COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
                       .fun=function(x) as.data.frame(t(x)))
names(v2_gkg_counts) <- paste("V1COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                             "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                             "GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                              sep="")
head(v2_gkg_counts)
v2_gkg <- cbind(v2_gkg, v2_gkg_counts)
v2_gkg <- select(v2_gkg, -c("V1COUNTS"))
head(v2_gkg)

## V2.1COUNTS
v21_gkg_counts <- ldply(.data=lapply(v2_gkg$V2.1COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
                        .fun=function(x) as.data.frame(t(x)))
names(v21_gkg_counts) <- paste("V2.1COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                                "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                                "GEO_LAT","GEO_LONG","GEO_FEATUREID","OFFSET"),
                                sep="")
head(v21_gkg_counts)
v2_gkg <- cbind(v2_gkg, v21_gkg_counts)
v2_gkg <- select(v2_gkg, -c("V2.1COUNTS"))
head(v2_gkg)

## V1THEMES
v2_gkg$V1THEMES <- lapply(v2_gkg$V1THEMES, function(x) str_split(x, ";")[[1]])
v2_gkg$V1THEMES <- lapply(v2_gkg$V1THEMES, function(x) x[-length(x)])
head(v2_gkg)

## V2ENHANCEDTHEMES
v2_gkg$V2ENHANCEDTHEMES <- lapply(v2_gkg$V2ENHANCEDTHEMES, 
                                  function(x) {
                                    # q is a list of row vectors for the data.frame of enhanced 
                                    # themes for this one row in the gkg dataset
                                    q <- lapply(str_split(x, ";")[[1]], function(y) str_split(y, ",")[[1]])
                                    # eliminate the few malformed rows, ones with more or less than
                                    # two elements
                                    q <- q[sapply(q, length)==2]
                                    # if no rows are left, return NA instead of a data.frame
                                    if(length(q)==0) return(NA)
                                    else {
                                      # form the data.frame from the row vectors
                                      out_x <- ldply(q)
                                      # set the names for the data.frame
                                      names(out_x) <- c("THEME","OFFSET")
                                      return(out_x)
                                    }
                                  })

## V1LOCATIONS
v2_gkg$V1LOCATIONS <- lapply(v2_gkg$V1LOCATIONS, 
                             function(x) {
                               # q is a list of row vectors for the data.frame of locations
                               # for this one row in the gkg dataset
                               q <- lapply(str_split(x, ";")[[1]], function(y) str_split(y, "#")[[1]])
                               # eliminate the few malformed rows, ones with more or less than
                               # seven elements
                               q <- q[sapply(q, length)==7]
                               # if no rows are left, return NA instead of a data.frame
                               if(length(q)==0) return(NA)
                               else {
                                 # form the data.frame from the row vectors
                                 out_x <- ldply(q)
                                 # set the names for the data.frame
                                 names(out_x) <- c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                                   "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID")
                                 return(out_x)
                               }
                             })

## V2ENHANCEDLOCATIONS
v2_gkg$V2ENHANCEDLOCATIONS <- lapply(v2_gkg$V2ENHANCEDLOCATIONS, 
                                     function(x) {
                                       # q is a list of row vectors for the data.frame of locations
                                       # for this one row in the gkg dataset
                                       q <- lapply(str_split(x, ";")[[1]], function(y) str_split(y, "#")[[1]])
                                       # eliminate the few malformed rows, ones with more or less than
                                       # nine elements
                                       q <- q[sapply(q, length)==9]
                                       # if no rows are left, return NA instead of a data.frame
                                       if(length(q)==0) return(NA)
                                       else {
                                         # form the data.frame from the row vectors
                                         out_x <- ldply(q)
                                         # set the names for the data.frame
                                         names(out_x) <- c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                                           "GEOADM1CODE","GEOADM2code","GEO_LAT",
                                                           "GEO_LONG","GEO_FEATUREID","OFFSET")
                                         return(out_x)
                                       }
                                     })

## V1PERSONS
