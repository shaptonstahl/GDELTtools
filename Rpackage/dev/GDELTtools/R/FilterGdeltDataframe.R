FilterGdeltDataframe <- function(x, 
                                 verbose,
                                 row_filter,
                                 ...) {
  # 'or' within values for a field, 'and' across fields
  #
  # ex: FilterGdeltDataframe(my_df, list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"))
  # This keeps rows with (ActionGeo_ADM1Code=NI AND ActionGeo_CountryCode=US) OR
  #   (ActionGeo_ADM1Code=US AND ActionGeo_CountryCode=US)
  
  if(verbose) cat("Filtering\n")
  x <- dplyr::filter(x, {{row_filter}})
  if(0==...length()) {
    cat("Selecting all columns\n\n")
    return(x)
  } else {
    cat("Selecting some columns using",...length(),"argument(s)\n")
    return( dplyr::select(x, ...) )
  }
}
