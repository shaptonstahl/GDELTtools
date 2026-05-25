# Given a dataframe, tibble, or matrix, create a unique id for each row.
# The salt is intended to be some unique identifier for the object
# such as its file name to prevent collisions between two objects.

CreatePrimaryKey <- function(x, salt="") {
  sapply(1:nrow(x), function(row_of_x) 
    digest::digest( list(salt, x[row_of_x,]), algo="md5", serializeVersion=3))
}