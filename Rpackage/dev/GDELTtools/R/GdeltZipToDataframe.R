# Unzips and loads to a dataframe a given zipped data file. Parses elements that
# themselves contain vectors or dataframes.

GdeltZipToDataframe <- function(file_w_path, 
                                version,
                                data_type,
                                v1_daily=FALSE, 
                                verbose=TRUE) {
  
  if(1==version) {
    if("events"==data_type) {
      gdelt_col_names <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED")
      gdelt_year_col_types <- "iciincccccccccccccccccccclccciniiinicccnncicccnncicccnncc"
      
      if(v1_daily) {
        gdelt_col_names <- c(gdelt_col_names, "SOURCEURL")
        gdelt_year_col_types <- paste(gdelt_year_col_types, "c", sep="")
      }
      if(verbose) cat("Ingesting", file_w_path, "\n")
      suppressWarnings(
        out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]),
                          col_names=gdelt_col_names, delim="\t",
                          col_types = gdelt_year_col_types)
      )
      out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))
      if(!v1_daily) {
        out$SOURCEURL <- as.character(NA)
      }
      out$SQLDATE <- as.Date(out$SQLDATE, format="%Y%m%d")
      out$DATEADDED <- as.Date(out$DATEADDED, format="%Y%m%d")
    } else if("gkg"==data_type) {
      if(verbose) cat("Ingesting", file_w_path, "\n")
      suppressWarnings(
        out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]), 
                          delim="\t", col_types="ciccccccccc")
      )
      out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))
      out$DATE <- as.Date(out$DATE, format="%Y%m%d")

      gkg_counts <- ldply(.data=lapply(out$COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
                          .fun=function(x) as.data.frame(t(x)))
      names(gkg_counts) <- paste("COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                              "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                              "GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                                 sep="")
      out <- cbind(out, gkg_counts)
      out <- select(out, -c("COUNTS"))

      out$THEMES <- SubList(out$THEMES)

      out$LOCATIONS <- SubTable(field=out$LOCATIONS, 
                                col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                            "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                                col_delim="#")

      out$PERSONS <- SubList(out$PERSONS)
      
      out$ORGANIZATIONS <- SubList(out$ORGANIZATIONS)
      
      gkg_tone <- ldply(.data=out$TONE, .fun=function(x) str_split(x, ",")[[1]])
      names(gkg_tone) <- paste("TONE_", c("TONE","POS_SCORE","NEG_SCORE","POLARITY",
                                          "ACTIVITY_REF_DENSITY","SELF_GROUP_REF_DENSITY"),
                               sep="")
      out <- cbind(out, gkg_tone)
      out <- select(out, -c("TONE"))

      out$CAMEOEVENTIDS <- SubList(out$CAMEOEVENTIDS, row_delim=",")
      
      out$SOURCES <- SubList(out$SOURCES)
      
      out$SOURCEURLS <- SubList(out$SOURCEURLS, row_delim="<UDIV>")
      
    } else if("gkgcounts"==data_type) {
      if(verbose) cat("Ingesting", file_w_path, "\n")
      suppressWarnings(
        out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]), 
                          delim="\t", col_types="cicncicccnncccc")
      )
      out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))
      out$DATE <- as.Date(out$DATE, format="%Y%m%d")

      out$CAMEOEVENTIDS <- SubList(out$CAMEOEVENTIDS, row_delim=",")
      
      out$SOURCES <- SubList(out$SOURCES)
      
      out$SOURCEURLS <- SubList(out$SOURCEURLS, row_delim="<UDIV>")
    } else {
      stop("GdeltZipToDataframe: For v1 data_type mut be events, gkg, or gkgcounts")
    }
  } else if(2==version) {
    if(data_type=="events") {
      gdelt_v2_event_col_types <- "iciincccccccccccccccccccclccciniiiniccccnnciccccnnciccccnnccc"
      gdelt_v2_event_col_names <- c("GlobalEventID", "Day", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_ADM2Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_ADM2Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_ADM2Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED", "SOURCEURL")
      
      if(verbose) cat("Ingesting", file_w_path, "\n")
      suppressWarnings(
        out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]), 
                          delim="\t", col_names=gdelt_v2_event_col_names,
                          col_types=gdelt_v2_event_col_types)
      )
      out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))
      out$Day <- as.Date(out$Day, format="%Y%m%d")
      out$DATEADDED <- as.POSIXct(out$DATEADDED, format="%Y%m%d%H%M%S", tz="UTC")
    } else if("gkg"==data_type) {
      v2_gkg_col_names <- c("GKGRECORDID","V2.1DATE","V2SOURCECOLLECTIONIDENTIFIER","V2SOURCECOMMONNAME",
                            "V2DOCUMENTIDENTIFIER","V1COUNTS","V2.1COUNTS","V1THEMES",
                            "V2ENHANCEDTHEMES","V1LOCATIONS","V2ENHANCEDLOCATIONS","V1PERSONS",
                            "V2ENHANCEDPERSONS","V1ORGANIZATIONS","V2ENHANCEDORGANIZATIONS","V1.5TONE",
                            "V2.1ENHANCEDDATES","V2GCAM","V2.1SHARINGIMAGE","V2.1RELATEDIMAGES",
                            "V2.1SOCIALIMAGEEMBEDS","V2.1SOCIALVIDEOEMBEDS","V2.1QUOTATIONS",
                            "V2.1ALLNAMES","V2.1AMOUNTS","V2.1TRANSLATIONINFO","V2EXTRASXML")
      v2_gkg_col_types <- c("ccicccccccccccccccccccccccc")
      
      if(verbose) cat("Ingesting", file_w_path, "\n")
      suppressWarnings(
        out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]), 
                          delim="\t", col_names=v2_gkg_col_names,
                          col_types=v2_gkg_col_types)
      )
      out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))
      ## V2.1DATE
      out$V2.1DATE <- as.Date(out$V2.1DATE, format="%Y%m%d")
      ## V1COUNTS
      v2_gkg_counts <- ldply(.data=lapply(out$V1COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
                             .fun=function(x) as.data.frame(t(x)))
      names(v2_gkg_counts) <- paste("V1COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                                   "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                                   "GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                                    sep="")
      out <- cbind(out, v2_gkg_counts)
      out <- select(out, -c("V1COUNTS"))
      ## V2.1COUNTS
      v21_gkg_counts <- ldply(.data=lapply(out$V2.1COUNTS, function(x) str_split(str_split(x, ";")[[1]], "#")[[1]]), 
                              .fun=function(x) as.data.frame(t(x)))
      names(v21_gkg_counts) <- paste("V2.1COUNTS_", c("COUNTTYPE","NUMBER","OBJECTTYPE","GEO_TYPE",
                                                      "GEO_FULLNAME","GEO_COUNTRYCODE","GEOADM1CODE",
                                                      "GEO_LAT","GEO_LONG","GEO_FEATUREID","OFFSET"),
                                     sep="")
      out <- cbind(out, v21_gkg_counts)
      out <- select(out, -c("V2.1COUNTS"))
      ## V1THEMES
      out$V1THEMES <- SubList(out$V1THEMES)
      ## V2ENHANCEDTHEMES
      out$V2ENHANCEDTHEMES <- SubTable(field=out$V2ENHANCEDTHEMES, 
                                          col_names=c("THEME","OFFSET"))
                                       col_names=c("THEME","OFFSET"),
                                       col_types="ci")
      ## V1LOCATIONS
      out$V1LOCATIONS <- SubTable(field=out$V1LOCATIONS, 
                                     col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                                 "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                                     col_delim="#")
                                  col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                              "GEOADM1CODE","GEO_LAT","GEO_LONG","GEO_FEATUREID"),
                                  col_types="icccnnc",
                                  col_delim="#")
      ## V2ENHANCEDLOCATIONS
      out$V2ENHANCEDLOCATIONS <- SubTable(field=out$V2ENHANCEDLOCATIONS, 
                                             col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                                         "GEOADM1CODE","GEOADM2CODE","GEO_LAT",
                                                         "GEO_LONG","GEO_FEATUREID","OFFSET"),
                                             col_delim="#")
                                          col_names=c("GEO_TYPE","GEO_FULLNAME","GEO_COUNTRYCODE",
                                                      "GEOADM1CODE","GEOADM2CODE","GEO_LAT",
                                                      "GEO_LONG","GEO_FEATUREID","OFFSET"),
                                          col_types="iccccnnci",
                                          col_delim="#")
      ## V1PERSONS
      out$V1PERSONS <- SubList(out$V1PERSONS)
      ## V2ENHANCEDPERSONS
      out$V2ENHANCEDPERSONS <- SubTable(field=out$V2ENHANCEDPERSONS, 
                                           col_names=c("PERSON","OFFSET"))
                                        col_names=c("PERSON","OFFSET"),
                                        col_types="ci")
      ## V1ORGANIZATIONS
      out$V1ORGANIZATIONS <- SubList(out$V1ORGANIZATIONS)
      ## V2ENHANCEDORGANIZATIONS
      out$V2ENHANCEDORGANIZATIONS <- SubTable(field=out$V2ENHANCEDORGANIZATIONS, 
                                                 col_names=c("ORGANIZATION","OFFSET"))
                                              col_names=c("ORGANIZATION","OFFSET"),
                                              col_types="ci")
      ## V1.5TONE
      v1.5_gkg_tone <- ldply(.data=out$V1.5TONE, .fun=function(x) str_split(x, ",")[[1]])
      names(v1.5_gkg_tone) <- paste("V1.5TONE_", c("TONE","POS_SCORE","NEG_SCORE","POLARITY",
                                                   "ACTIVITY_REF_DENSITY","SELF_GROUP_REF_DENSITY",
                                                   "WORD_COUNT"),
                                    sep="")
      out <- cbind(out, v1.5_gkg_tone)
      out <- select(out, -c("V1.5TONE"))
      ## V2.1ENHANCEDDATES
      out$V2.1ENHANCEDDATES <- SubTable(field=out$V2.1ENHANCEDDATES, 
                                           col_names=c("DATE_RESOLUTION","MONTH","DAY",
                                                       "YEAR","OFFSET"),
                                           col_delim="#")
                                        col_names=c("DATE_RESOLUTION","MONTH","DAY",
                                                    "YEAR","OFFSET"),
                                        col_types="iiiii",
                                        col_delim="#")
      ## V2GCAM
      out$V2GCAM <- SubTable(field=out$V2GCAM, 
                                col_names=c("DIMENSION","SCORE"),
                                row_delim=",",
                                col_delim=":")
                             col_names=c("DIMENSION","SCORE"),
                             col_types="cn",
                             row_delim=",",
                             col_delim=":")
      ## V2.1RELATEDIMAGES
      out$V2.1RELATEDIMAGES <- SubList(out$V2.1RELATEDIMAGES)
      ## V2.1SOCIALIMAGEEMBEDS
      out$V2.1SOCIALIMAGEEMBEDS <- SubList(out$V2.1SOCIALIMAGEEMBEDS)
      ## V2.1SOCIALVIDEOEMBEDS
      out$V2.1SOCIALVIDEOEMBEDS <- SubList(out$V2.1SOCIALVIDEOEMBEDS)
      ## V2.1QUOTATIONS
      out$V2.1QUOTATIONS <- SubTable(field=out$V2.1QUOTATIONS,
                                        col_names=c("OFFSET","LENGTH","VERB","QUOTE"),
                                        row_delim="#",
                                        col_delim="\\|")
                                     col_names=c("OFFSET","LENGTH","VERB","QUOTE"),
                                     col_types="iicc",
                                     row_delim="#",
                                     col_delim="\\|")
      ## V2.1ALLNAMES
      out$V2.1ALLNAMES <- SubTable(field=out$V2.1ALLNAMES,
                                      col_names=c("NAME","OFFSET"))
                                   col_names=c("NAME","OFFSET"),
                                   col_types="ci")
      ## V2.1AMOUNTS
      out$V2.1AMOUNTS <- SubTable(field=out$V2.1AMOUNTS,
                                     col_names=c("AMOUNT","OBJECT","OFFSET"))
                                  col_names=c("AMOUNT","OBJECT","OFFSET"),
                                  col_types="nci")
      ## V2.1TRANSLATIONINFO
      v2_gkg_TI <- ldply(str_split(out$V2.1TRANSLATIONINFO, ";"), function(x) {
        if(is.na(x[1])) return(c(NA,NA))
        else {
          v <- sapply(x, function(y) {
            element <- str_split(y, ":")[[1]]
            return(element[2])
          })
          return(v)
        }
      })
      names(v2_gkg_TI) <- paste("V2.1TRANSLATIONINFO_", c("SRCLC","ENG"), sep="")
      out <- cbind(out, v2_gkg_TI)
      out <- select(out, -c("V2.1TRANSLATIONINFO"))
    } else if("mentions"==data_type) {
      v2_mentions_col_names <- c("GlobalEventID","EventTimeDate","MentionTimeDate",
                                 "MentionType","MentionSourceName","MentionIdentifier",
                                 "SentenceID","Actor1CharOffset","Actor2CharOffset",
                                 "ActionCharOffset","InRawText","Confidence","MentionDocLen",
                                 "MentionDocTone","MentionDocTranslationInfo","Extras")
      v2_mentions_col_types <- c("cccicciiiilnincc")
      
      if(verbose) cat("Ingesting", file_w_path, "\n")
      suppressWarnings(
        out <- read_delim(unz(file_w_path, unzip(file_w_path, list=TRUE)$Name[1]), 
                          delim="\t", col_names=v2_mentions_col_names,
                          col_types=v2_mentions_col_types)
        
      )
      out$GDELTtools_key <- CreatePrimaryKey(out, salt=basename(file_w_path))
      out$EventTimeDate <- as.POSIXct(out$EventTimeDate, format="%Y%m%d%H%M%S", tz="UTC")
      out$MentionTimeDate <- as.POSIXct(out$MentionTimeDate, format="%Y%m%d%H%M%S", tz="UTC")
      ## MentionDocTranslationInfo
      v2_mentions_TI <- ldply(str_split(out$MentionDocTranslationInfo, ";"), function(x) {
        if(is.na(x[1])) return(c(NA,NA))
        else {
          v <- sapply(x, function(y) {
            element <- str_split(y, ":")[[1]]
            return(element[2])
          })
          return(v)
        }
      })
      names(v2_mentions_TI) <- paste("MentionDocTranslationInfo_", c("SRCLC","ENG"), sep="")
      out <- cbind(out, v2_mentions_TI)
      out <- select(out, -c("MentionDocTranslationInfo"))
    } else {
      stop("GdeltZipToDataframe: For v2 the data_type must be events, gkg, or mentions")
    }
  } else {
    stop("GdeltZipToDataframe: version must be 1 or 2")
  }
  
  return(out)
}