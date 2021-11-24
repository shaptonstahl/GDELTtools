FilterGdeltDataframe <- function(x, 
                                 filter, 
                                 allow_wildcards=FALSE, 
                                 use_regex=FALSE,
                                 verbose=TRUE) {
  # 'or' within values for a field, 'and' across fields
  #
  # ex: FilterGdeltDataframe(my_df, list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"))
  # This keeps rows with (ActionGeo_ADM1Code=NI AND ActionGeo_CountryCode=US) OR
  #   (ActionGeo_ADM1Code=US AND ActionGeo_CountryCode=US)
  if(verbose) cat("Filtering\n\n")
  if(use_regex) {
    filter_results <- laply(1:length(filter), function(fi) {
      field_results <- laply(.data=filter[[fi]], .fun=function(v) {
        grepl(v, x[names(filter)[fi]][,1])
      }, .drop=FALSE)
      if(is.array(field_results)) return(apply(field_results, 2, any))
      else return(field_results)
    })
  } else if(allow_wildcards) {
    filter_results <- laply(1:length(filter), function(fi) {
      field_results <- laply(.data=filter[[fi]], .fun=function(v) {
        v <- gsub("*", "[:alnum:]*", v, fixed=TRUE)
        grepl(v, x[names(filter)[fi]][,1])
      }, .drop=FALSE)
      if(is.array(field_results)) return(apply(field_results, 2, any))
      else return(field_results)
    })
  } else {
    filter_results <- laply(1:length(filter), function(fi) {
      field_results <- laply(.data=filter[[fi]], .fun=function(v) x[names(filter)[fi]]==v, .drop=FALSE)
      if(is.array(field_results)) return(apply(field_results, 2, any))
      else return(field_results)
    })
  }
  
  if(is.array(filter_results)) rows_to_keep <- apply(filter_results, 2, all)
  else rows_to_keep <- filter_results
  
  out <- x[rows_to_keep,]
  
  # remove NA values for filtered fields
  for(i in 1:length(filter)) {
    keep_rows <- !is.na(out[,names(filter)[i]])
    out <- out[keep_rows,]
  }
  
  return(out)
}
