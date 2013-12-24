#' Verify the integrity of a GDELT data file
#'
#' Compares the MD5 hash of a downloaded file to the known hash provided
#' on the server.
#' 
#' @aliases IsValidGDELT
#' @param f character, file name of GDELT file to be validated.
#' @param local.folder character, path to the file to be validated.
#' @return logical, TRUE if the file is intact.
#' @export
#' @references
#' GDELT: Global Data on Events, Location and Tone, 1979-2012.  
#' Presented at the 2013 meeting of the International Studies Association
#' in San Francisco, CA.
#' \url{http://gdelt.utdallas.edu/}
#' @author 
#' \tabular{ll}{
#'   Stephen R. Haptonstahl \tab \email{srh@@haptonstahl.org}\cr
#' }
#' @examples
#' \dontrun{
#' IsValidGDELT("1979.zip", "c:/gdeltdata")} 
IsValidGDELT <- function(f,
                         local.folder) {
  
  historical.names.zips <- c("201303.zip", "201302.zip", "201301.zip", "201212.zip", "201211.zip", "201210.zip", "201209.zip", "201208.zip", "201207.zip", "201206.zip", "201205.zip", "201204.zip", "201203.zip", "201202.zip", "201201.zip", "201112.zip", "201111.zip", "201110.zip", "201109.zip", "201108.zip", "201107.zip", "201106.zip", "201105.zip", "201104.zip", "201103.zip", "201102.zip", "201101.zip", "201012.zip", "201011.zip", "201010.zip", "201009.zip", "201008.zip", "201007.zip", "201006.zip", "201005.zip", "201004.zip", "201003.zip", "201002.zip", "201001.zip", "200912.zip", "200911.zip", "200910.zip", "200909.zip", "200908.zip", "200907.zip", "200906.zip", "200905.zip", "200904.zip", "200903.zip", "200902.zip", "200901.zip", "200812.zip", "200811.zip", "200810.zip", "200809.zip", "200808.zip", "200807.zip", "200806.zip", "200805.zip", "200804.zip", "200803.zip", "200802.zip", "200801.zip", "200712.zip", "200711.zip", "200710.zip", "200709.zip", "200708.zip", "200707.zip", "200706.zip", "200705.zip", "200704.zip", "200703.zip", "200702.zip", "200701.zip", "200612.zip", "200611.zip", "200610.zip", "200609.zip", "200608.zip", "200607.zip", "200606.zip", "200605.zip", "200604.zip", "200603.zip", "200602.zip", "200601.zip", "2005.zip", "2004.zip", "2003.zip", "2002.zip", "2001.zip", "2000.zip", "1999.zip", "1998.zip", "1997.zip", "1996.zip", "1995.zip", "1994.zip", "1993.zip", "1992.zip", "1991.zip", "1990.zip", "1989.zip", "1988.zip", "1987.zip", "1986.zip", "1985.zip", "1984.zip", "1983.zip", "1982.zip", "1981.zip", "1980.zip", "1979.zip")
  if(f %in% historical.names.zips) {
    md5.url <- "http://gdelt.umn.edu/data/backfiles/backfile.md5"
  } else {
    md5.url <- "http://gdelt.umn.edu/data/dailyupdates/dailyupdates.md5"
  }
  
  md5.df <- read.delim(md5.url, sep=" ", header=FALSE)
  this.md5 <- md5.df[ md5.df[,ncol(md5.df)]==f ,1]
  if(length(this.md5) != 1) {
    warning("Unable to find MD5 for ", f)
    return(FALSE)
  }
  
  observed.md5 <- md5sum(paste(local.folder, "/", f, sep=""))
  
  return( observed.md5 == this.md5 )
}
