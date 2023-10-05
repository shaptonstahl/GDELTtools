# Given a field in a GDELT dataframe, splits each element into rows by row_delim
# then splits each row into fields by col_delim.

SubTable <- function(field, col_names, row_delim=";", col_delim=",") {
  lapply(field,
         function(x) {
           # q is a list of row vectors for the data.frame of locations
           # for this one row in the gkg dataset
           q <- lapply(str_split(x, row_delim)[[1]], function(y) str_split(y, col_delim)[[1]])
           # eliminate the few malformed rows, ones with more or less than
           # seven elements
           q <- q[sapply(q, length)==length(col_names)]
           # if no rows are left, return NA instead of a data.frame
           if(length(q)==0) return(NA)
           else {
             # form the data.frame from the row vectors
             out_x <- ldply(q)
             # set the names for the data.frame
             names(out_x) <- col_names
             return(out_x)
           }
         })
}
