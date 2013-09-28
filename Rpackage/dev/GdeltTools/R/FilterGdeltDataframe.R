#' Filter a data.frame by field name
#'
#' This is not specific to Gdelt
#' 
#' @param x data.frame, imported from a GDELT zip file.
#' @param filter list, see details.
#' @param allow.wildcards logical, Do you want to use wildcards in \code{filter}?
#' @param use.regex logical, Do you want to use regular expressions in \code{filter}?
#' @return data.frame, filtered on selected values
#' @references
#' \url{http://gdelt.utdallas.edu/}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' \dontrun{df.filtered <- GdeltZipToDataframe(df, filter=list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"))}
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
