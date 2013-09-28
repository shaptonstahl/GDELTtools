library(plyr)

GdeltZipToDataframe <- function(f) {
  gdelt.colClasses<-c(GLOBALEVENTID="integer", SQLDATE="integer", MonthYear="integer", Year="integer", FractionDate="numeric", Actor1Code="character", Actor1Name="character", Actor1CountryCode="character", Actor1KnownGroupCode="character", Actor1EthnicCode="character", Actor1Religion1Code="character", Actor1Religion2Code="character", Actor1Type1Code="character", Actor1Type2Code="character", Actor1Type3Code="character", Actor2Code="character", Actor2Name="character", Actor2CountryCode="character", Actor2KnownGroupCode="character", Actor2EthnicCode="character", Actor2Religion1Code="character", Actor2Religion2Code="character", Actor2Type1Code="character", Actor2Type2Code="character", Actor2Type3Code="character", IsRootEvent="integer", EventCode="character", EventBaseCode="character", EventRootCode="character", QuadClass="integer", GoldsteinScale="numeric", NumMentions="integer", NumSources="integer", NumArticles="integer", AvgTone="numeric", Actor1Geo_Type="integer", Actor1Geo_FullName="character", Actor1Geo_CountryCode="character", Actor1Geo_ADM1Code="character", Actor1Geo_Lat="numeric", Actor1Geo_Long="numeric", Actor1Geo_FeatureID="integer", Actor2Geo_Type="integer", Actor2Geo_FullName="character", Actor2Geo_CountryCode="character", Actor2Geo_ADM1Code="character", Actor2Geo_Lat="numeric", Actor2Geo_Long="numeric", Actor2Geo_FeatureID="integer", ActionGeo_Type="integer", ActionGeo_FullName="character", ActionGeo_CountryCode="character", ActionGeo_ADM1Code="character", ActionGeo_Lat="numeric", ActionGeo_Long="numeric", ActionGeo_FeatureID="integer", DATEADDED="integer")
  out <- read.delim(unz(f, unzip(f, list=TRUE)$Name[1]), header=FALSE, stringsAsFactors=FALSE, colClasses=gdelt.colClasses)
  if(57==ncol(out)) out <- data.frame(out, "", stringsAsFactors=FALSE)
  names(out) <- c("GLOBALEVENTID", "SQLDATE", "MonthYear", "Year", "FractionDate", "Actor1Code", "Actor1Name", "Actor1CountryCode", "Actor1KnownGroupCode", "Actor1EthnicCode", "Actor1Religion1Code", "Actor1Religion2Code", "Actor1Type1Code", "Actor1Type2Code", "Actor1Type3Code", "Actor2Code", "Actor2Name", "Actor2CountryCode", "Actor2KnownGroupCode", "Actor2EthnicCode", "Actor2Religion1Code", "Actor2Religion2Code", "Actor2Type1Code", "Actor2Type2Code", "Actor2Type3Code", "IsRootEvent", "EventCode", "EventBaseCode", "EventRootCode", "QuadClass", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "Actor1Geo_Type", "Actor1Geo_FullName", "Actor1Geo_CountryCode", "Actor1Geo_ADM1Code", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor1Geo_FeatureID", "Actor2Geo_Type", "Actor2Geo_FullName", "Actor2Geo_CountryCode", "Actor2Geo_ADM1Code", "Actor2Geo_Lat", "Actor2Geo_Long", "Actor2Geo_FeatureID", "ActionGeo_Type", "ActionGeo_FullName", "ActionGeo_CountryCode", "ActionGeo_ADM1Code", "ActionGeo_Lat", "ActionGeo_Long", "ActionGeo_FeatureID", "DATEADDED", "SOURCEURL")
  return(out)
}

FilterGdeltDataframe <- function(x, filter, allow.wildcards=FALSE, use.regex=FALSE) {
  # 'or' within values for a field, 'and' across fields
  #
  # ex: FilterGdeltDataframe(my.df, list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"))
  # This keeps rows with (ActionGeo_ADM1Code=NI AND ActionGeo_CountryCode=US) OR
  #   (ActionGeo_ADM1Code=US AND ActionGeo_CountryCode=US)
  if(use.regex) {
    filter.results <- laply(1:length(filter), function(fi) {
      apply(laply(.data=filter[[fi]], .fun=function(v) {
        grepl(v, x[names(filter)[fi]][,1])
      }, .drop=FALSE), 2, any)
    })
  } else if(allow.wildcards) {
    filter.results <- laply(1:length(filter), function(fi) {
      apply(laply(.data=filter[[fi]], .fun=function(v) {
        v <- gsub("*", "[:alnum:]*", v, fixed=TRUE)
        grepl(v, x[names(filter)[fi]][,1])
      }, .drop=FALSE), 2, any)
    })
  } else {
    filter.results <- laply(1:length(filter), function(fi) {
      apply(laply(.data=filter[[fi]], .fun=function(v) x[names(filter)[fi]]==v, .drop=FALSE), 2, any)
    })
  }
  if(is.array(filter.results)) rows.to.keep <- apply(filter.results, 2, all)
  else rows.to.keep <- filter.results
  return(x[rows.to.keep,])
}

# g79 <- GdeltZipToDataframe("1979.zip")
# test.filter = list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US")
# g79.filtered <- FilterGdeltDataframe(g79, test.filter)

# g79.filtered <- FilterGdeltDataframe(g79, list(ActionGeo_ADM1Code="US*"), allow.wildcards=TRUE)
# table(g79.filtered$ActionGeo_ADM1Code)

# g79.filtered <- FilterGdeltDataframe(g79, list(ActionGeo_ADM1Code="US.*"), use.regex=TRUE)
# table(g79.filtered$ActionGeo_ADM1Code)

FileInfo <- function(path, include.dirs=FALSE) {
  # Returns info on files in path in a data.frame
  info.on.files <- ldply(paste(path, "/", dir(path), sep=""), file.info)
  info.on.files <- data.frame(name=dir(path), info.on.files, stringsAsFactors=FALSE)
  if(!include.dirs) info.on.files <- info.on.files[!info.on.files$isdir,]
  return(info.on.files)
}

DownloadGdelt <- function(f,
                          local.folder,
                          max.local.mb,
                          historical.url.root="http://gdelt.utdallas.edu/data/backfiles/",
                          daily.url.root="http://gdelt.utdallas.edu/data/dailyupdates/",
                          verbose=FALSE) {
  # Dowloads a single file, then removes files if necessary to get under max.local.mb
  # Returns TRUE if file downloaded successfully, FALSE otherwise
  # DOES NOT GIVE A WARNING if non-gdelt files are in the local.folder
  
  # Guardians
  if(!missing(max.local.mb)) stopifnot(max.local.mb >= 0)
  # Add guardian to ensure URLs end with a slash
  # Add guardian to ensure local.folder does NOT end with a slash or backslash
  
  # Constants
  historical.names.zips <- c("201303.zip", "201302.zip", "201301.zip", "201212.zip", "201211.zip", "201210.zip", "201209.zip", "201208.zip", "201207.zip", "201206.zip", "201205.zip", "201204.zip", "201203.zip", "201202.zip", "201201.zip", "201112.zip", "201111.zip", "201110.zip", "201109.zip", "201108.zip", "201107.zip", "201106.zip", "201105.zip", "201104.zip", "201103.zip", "201102.zip", "201101.zip", "201012.zip", "201011.zip", "201010.zip", "201009.zip", "201008.zip", "201007.zip", "201006.zip", "201005.zip", "201004.zip", "201003.zip", "201002.zip", "201001.zip", "200912.zip", "200911.zip", "200910.zip", "200909.zip", "200908.zip", "200907.zip", "200906.zip", "200905.zip", "200904.zip", "200903.zip", "200902.zip", "200901.zip", "200812.zip", "200811.zip", "200810.zip", "200809.zip", "200808.zip", "200807.zip", "200806.zip", "200805.zip", "200804.zip", "200803.zip", "200802.zip", "200801.zip", "200712.zip", "200711.zip", "200710.zip", "200709.zip", "200708.zip", "200707.zip", "200706.zip", "200705.zip", "200704.zip", "200703.zip", "200702.zip", "200701.zip", "200612.zip", "200611.zip", "200610.zip", "200609.zip", "200608.zip", "200607.zip", "200606.zip", "200605.zip", "200604.zip", "200603.zip", "200602.zip", "200601.zip", "2005.zip", "2004.zip", "2003.zip", "2002.zip", "2001.zip", "2000.zip", "1999.zip", "1998.zip", "1997.zip", "1996.zip", "1995.zip", "1994.zip", "1993.zip", "1992.zip", "1991.zip", "1990.zip", "1989.zip", "1988.zip", "1987.zip", "1986.zip", "1985.zip", "1984.zip", "1983.zip", "1982.zip", "1981.zip", "1980.zip", "1979.zip")
  if(f %in% historical.names.zips) {
    file.is.historical <- TRUE
    url.root <- historical.url.root
  } else {
    file.is.historical <- FALSE
    url.root <- daily.url.root
  }
  if(missing(local.folder)) local.folder <- tempdir()
  
  # Download the file
  result <- download.file(url=paste(url.root, f, sep=""),
                          destfile=paste(local.folder, "/", f, sep=""),
                          quiet=!verbose)
  if(0 != result) return(FALSE)
  
  # Clean up if necessary
  if(!missing(max.local.mb)) {
    info.on.files <- FileInfo(local.folder)
    mb.currently.stored <- sum(info.on.files$size, na.rm=TRUE) / 2^20
    #browser()
    while(mb.currently.stored > max.local.mb) {
      # delete file in folder accessed longest ago, BUT NOT CURRENT FILE
      info.on.files <- info.on.files[-which(dir(local.folder, include.dirs=FALSE)==f),]  # remove current file from consideration for deletion
      info.on.files <- info.on.files[info.on.files$size > 0,]  # remove size-zero files
      if(0 == nrow(info.on.files)) {
        # exit, because current file is the only file
        mb.currently.stored <- 0
      } else {
        old.file.ids <- which(min(info.on.files$atime, na.rm=TRUE)==info.on.files$atime)
        if(length(old.file.ids) < 1) stop("No local files to delete.")
        else del.file.id <- old.file.ids[1]
        
        file.remove(paste(local.folder, "/", dir(local.folder, include.dirs=FALSE)[del.file.id], sep=""))
        
        # update
        info.on.files <- FileInfo(local.folder)
        mb.currently.stored <- sum(info.on.files$size, na.rm=TRUE) / 2^20
      }
    }
  }
  return(TRUE)
}

# DownloadGdelt("1979.zip", max.local.mb=10, historical.url.root="http://dragon/gdelt/backfiles/")
# dir(tempdir())
# DownloadGdelt("1980.zip", max.local.mb=10, historical.url.root="http://dragon/gdelt/backfiles/")
# dir(tempdir())
# DownloadGdelt("1979.zip", max.local.mb=100, historical.url.root="http://dragon/gdelt/backfiles/")
# dir(tempdir())

LocalVersusRemote <- function(filelist, local.folder) {
  # Given a list of files, determines which ones are local and which are remote
  return( list(local=filelist[filelist %in% dir(local.folder)],
               remote=filelist[!(filelist %in% dir(local.folder))]) )
}

GetGdelt <- function(start.date,
                     end.date=start.date,
                     filter,
                     local.folder,
                     max.local.mb,
                     allow.wildcards=FALSE, 
                     use.regex=FALSE,
                     historical.url.root="http://gdelt.utdallas.edu/data/backfiles/",
                     daily.url.root="http://gdelt.utdallas.edu/data/dailyupdates/",
                     verbose=FALSE) {
  
  out.initialized <- FALSE
  
  # Determine file list based on dates
  source.files <- c("1979.zip", "1980.zip")
  cat("#####  Note: source.files is a static list for now  #####\n")
  
  # Ingest and filter local files
  for(this.file in LocalVersusRemote(filelist=source.files, local.folder=local.folder)$local) {
    new.data <- GdeltZipToDataframe(f=paste(local.folder, "/", this.file, sep=""))
    new.data <- FilterGdeltDataframe(x=new.data,
                                     filter=filter,
                                     allow.wildcards=allow.wildcards,
                                     use.regex=use.regex)
    if(out.initialized) {
      out <- rbind(out, new.data)
    } else {
      out <- new.data
      out.initialized <- TRUE
    }
  }
  
  # Download, ingest, and filter remote files
  for(this.file in LocalVersusRemote(filelist=source.files, local.folder=local.folder)$remote) {
    download.result <- DownloadGdelt(f=this.file,
                                     local.folder=local.folder,
                                     max.local.mb=max.local.mb,
                                     historical.url.root=historical.url.root,
                                     daily.url.root=daily.url.root,
                                     verbose=FALSE)
    new.data <- GdeltZipToDataframe(f=paste(local.folder, "/", this.file, sep=""))
    new.data <- FilterGdeltDataframe(x=new.data,
                                     filter=filter,
                                     allow.wildcards=allow.wildcards,
                                     use.regex=use.regex)
    if(out.initialized) {
      out <- rbind(out, new.data)
    } else {
      out <- new.data
      out.initialized <- TRUE
    }
  }
  
  # Filter one more time on dates
  
  return(out)
}

# gdelt.folder <- "c:/gdeltdata"

my.data <- GetGdelt(start.date="1979", 
                    end.date="1980", 
                    filter=list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"),
                    local.folder=gdelt.folder,
                    max.local.mb=100)
str(my.data)

