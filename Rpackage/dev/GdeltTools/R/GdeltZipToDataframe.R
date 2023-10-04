GdeltZipToDataframe <- function(f, 
                                version,
                                data_type,
                                daily=FALSE, 
                                verbose=TRUE) {
  
  if(1==version) {
    if("events"==data_type) {
      gdelt_col_names <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED")
      gdelt_year_col_types <- "iciincccccccccccccccccccclccciniiinicccnniicccnniicccnnic"
      
      if(daily) {
        gdelt_col_names <- c(gdelt_col_names, "SOURCEURL")
        gdelt_year_col_types <- paste(gdelt_year_col_types, "c", sep="")
      }
      if(verbose) cat("Ingesting", f, "\n")
      out <- read_delim(unz(paste(path, "/", f, sep=""), unzip(paste(path, "/", f, sep=""), 
                                                               list=TRUE)$Name[1]),
                        col_names=gdelt_col_names, delim="\t",
                        col_types = gdelt_year_col_types)
      
      if(!daily) {
        out$SOURCEURL <- as.character(NA)
      }
      out$SQLDATE <- as.Date(out$SQLDATE, format="%Y%m%d")
      out$DATEADDED <- as.Date(out$DATEADDED, format="%Y%m%d")
    } else if("gkg"==data_type) {
      out <- read_delim(unz(paste(path, "/", f, sep=""), 
                               unzip(paste(path, "/", f, sep=""), 
                                     list=TRUE)$Name[1]), 
                           delim="\t", col_types="ciccccccccc")
      out$DATE <- as.Date(out$DATE, format="%Y%m%d")

      gkg_counts <- ldply(.data=lapply(out$COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
                          .fun=function(x) as.data.frame(t(x)))
      names(gkg_counts) <- paste("COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                              "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                              "GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                                 sep="")
      out <- cbind(out, gkg_counts)
      out <- select(out, -c("COUNTS"))

      out$THEMES <- lapply(out$THEMES, function(x) str_split(x, ";")[[1]])
      out$THEMES <- lapply(out$THEMES, function(x) x[-length(x)])

      out$LOCATIONS <- lapply(out$LOCATIONS, 
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

      out$PERSONS <- lapply(out$PERSONS, function(x) str_split(x, ";")[[1]])
      
      out$ORGANIZATIONS <- lapply(out$ORGANIZATIONS, function(x) str_split(x, ";")[[1]])
      
      gkg_tone <- ldply(.data=out$TONE, .fun=function(x) str_split(x, ",")[[1]])
      names(gkg_tone) <- paste("TONE_", c("TONE","POS_SCORE","NEG_SCORE","POLARITY",
                                          "ACTIVITY_REF_DENSITY","SELF_GROUP_REF_DENSITY"),
                               sep="")
      out <- cbind(out, gkg_tone)
      out <- select(out, -c("TONE"))

      out$CAMEOEVENTIDS <- lapply(out$CAMEOEVENTIDS, function(x) str_split(x, ",")[[1]])
      
      out$SOURCES <- lapply(out$SOURCES, function(x) str_split(x, ";")[[1]])
      
      out$SOURCEURLS <- str_split(out$SOURCEURLS, "<UDIV>")
      
    } else if("gkgcounts"==data_type) {
      out <- read_delim(unz(paste(path, "/", f, sep=""), 
                                     unzip(paste(path, "/", f, sep=""), 
                                           list=TRUE)$Name[1]), 
                                 delim="\t", col_types="cicicicccnniccc")
      out$DATE <- as.Date(out$DATE, format="%Y%m%d")

      out$CAMEOEVENTIDS <- lapply(out$CAMEOEVENTIDS, function(x) str_split(x, ",")[[1]])
      
      out$SOURCES <- lapply(out$SOURCES, function(x) str_split(x, ";")[[1]])
      
      out$SOURCEURLS <- str_split(out$SOURCEURLS, "<UDIV>")
    } else {
      stop("GdeltZipToDataframe: For v1 data_type mut be events, gkg, or gkgcounts")
    }
  } else if(2==version) {
    if(date_type=="events") {
      gdelt_v2_event_col_types <- "iciincccccccccccccccccccclccciniiiniccccnnciccccnnciccccnnccc"
      gdelt_v2_event_col_names <- c("GlobalEventID", "Day", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_ADM2Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_ADM2Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_ADM2Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED", "SOURCEURL")
      
      out <- read_delim(unz(paste(path, "/", f, sep=""), 
                                  unzip(paste(path, "/", f, sep=""), 
                                        list=TRUE)$Name[1]), 
                              delim="\t", col_names=gdelt_v2_event_col_names,
                              col_types=gdelt_v2_event_col_types)
      out$Day <- as.Date(out$Day, format="%Y%m%d")
      out$DATEADDED <- as.POSIXct(out$DATEADDED, format="%Y%m%d%H%M%S", tz="UTC")
    } else if("gkg"==data_type) {
      
    } else if("mentions"==data_type) {
      
    } else {
      stop("GdeltZipToDataframe: For v2 the data_type must be events, gkg, or mentions")
    }
  } else {
    stop("GdeltZipToDataframe: version must be 1 or 2")
  }
  
  return(out)
}