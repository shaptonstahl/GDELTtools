# Given a column ("field") in a GDELT dataframe, return a vector of vectors
# containing the original elements split by row_delim. Removes empty elements.

SubList <- function(field, row_delim=";") {
  lapply(field, function(x) {
    if(is.na(x)) return(x)
    else {
      out <- str_split(x, row_delim)[[1]]
      out <- out[!(nchar(out)==0)]
      return(out)
    }
  })
}