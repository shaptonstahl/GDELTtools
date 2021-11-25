FilterGdeltDataframe <- function(x, 
                                 verbose=TRUE,
                                 row_filter,
                                 ...) {
  # 'or' within values for a field, 'and' across fields
  #
  # ex: FilterGdeltDataframe(my_df, list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"))
  # This keeps rows with (ActionGeo_ADM1Code=NI AND ActionGeo_CountryCode=US) OR
  #   (ActionGeo_ADM1Code=US AND ActionGeo_CountryCode=US)
  if(verbose) cat("Filtering\n\n")
  x <- filter(x, {{row_filter}})
  return( select(x, ...) )
}
