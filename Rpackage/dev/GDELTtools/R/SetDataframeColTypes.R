# Given a dataframe and a string of letters indicating col_types returns the
# same dataframe with columns converted to the desired types. The skip variable
# is a set of letters indicating which types to not set. This is usually "c"
# because the default column type is often character.

SetDataframeColTypes <- function(x, col_types, skip="", verbose=FALSE) {
  col_types <- str_split_1(col_types, "")
  skip <- str_split_1(skip, "")
  
  # character
  cols <- which(col_types == "c")
  if(length(cols)> 0 & !("c" %in% skip)) {
    if(verbose) cat("Converting character columns\n")
    x <- convert(x, chr(cols))
  }
  # integer
  cols <- which(col_types == "i")
  if(length(cols)> 0 & !("i" %in% skip)) {
    if(verbose) cat("Converting integer columns\n")
    x <- convert(x, int(cols))
  }
  # numeric
  cols <- which(col_types == "n")
  if(length(cols)> 0 & !("n" %in% skip)) {
    if(verbose) cat("Converting numeric columns\n")
    x <- convert(x, num(cols))
  }
  # double
  cols <- which(col_types == "d")
  if(length(cols)> 0 & !("d" %in% skip)) {
    if(verbose) cat("Converting double columns\n")
    x <- convert(x, dbl(cols))
  }
  # logical
  cols <- which(col_types == "l")
  if(length(cols)> 0 & !("l" %in% skip)) {
    if(verbose) cat("Converting logical columns\n")
    x <- convert(x, lgl(cols))
  }
  # factor
  cols <- which(col_types == "f")
  if(length(cols)> 0 & !("f" %in% skip)) {
    if(verbose) cat("Converting factor columns\n")
    x <- convert(x, fct(cols))
  }
  # date
  cols <- which(col_types == "D")
  if(length(cols)> 0 & !("D" %in% skip)) {
    if(verbose) cat("Converting Date columns\n")
    x <- convert(x, dte(cols))
  }
  # datetime
  cols <- which(col_types == "T")
  if(length(cols)> 0 & !("T" %in% skip)) {
    if(verbose) cat("Converting datetime columns\n")
    x <- convert(x, dtm(cols))
  }
  # time (t) not supported
  return(x)
}